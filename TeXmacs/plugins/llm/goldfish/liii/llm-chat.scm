;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(set! *load-path*
  (let ((user-path (string-append (getenv "TEXMACS_HOME_PATH") "/plugins/markdown/goldfish"))
        (sys-path (string-append (getenv "TEXMACS_PATH") "/plugins/markdown/goldfish")))
    (if (file-exists? user-path)
        (cons user-path *load-path*)
        (cons sys-path *load-path*))))

(define-library (liii llm-chat)
(import (liii base) (liii os) (liii lang) (liii path) (liii json) (liii http)
        (liii llm-data) (liii logging) (liii string) (liii markdown))
(export llm chat response2either)
(begin

(define log (logging "llm"))
(log :set-path! "/tmp/llm.log")

(define (headers key)
  `(
     ("Authorization" . ,(string-append "Bearer " key))
     ("Content-Type" . "application/json")
   )
)

(define (response2either r)
  (cond ((= (r 'status-code) 0)
         (left "HTTP状态码为0，请求未能发送到服务器，请检查是否联网或者代理设置是否正确"))
        ((= (r 'status-code) 200)
         (right r))
        (else
         (left ($ "HTTP状态码为" :+ (r 'status-code) :+ "，存在HTTP请求错误，请查看对应服务提供商的状态码对应的报错原因" :get)))))

(define state (make-stream-tokenizer))

(define (flush-markdown-block! content)
  (log :debug "Flushing markdown blocks content: " content)
  (let* ((final-state (stream-tokenizer-step content state))
         (blocks (stream-tokenizer-drain-blocks final-state)))
    (log :debug "----")
    (log :debug "Blocks: " (object->string blocks))
    ; (log :debug "Pending: "
    ;      (object->string (stream-tokenizer-pending-block
    ;                        final-state)))
    (log :debug "----")
    (set! state final-state)
    (for-each case-block blocks)))

(define (force-flush-markdown-block! content)
  (log :debug "Force flushing markdown blocks content: " content)
  (let* ((final-state (stream-tokenizer-step content state))
         (blocks (stream-tokenizer-drain-all final-state)))
    (set! state final-state)
    (for-each case-block blocks)))

(define (case-block block)
  (let ((type (car block))
        (content (cdr block)))
    (case type
      ((thematic-break) (begin (flush-verbatim "\n")
                               (flush-scheme '(document (hrule) ""))))
      ((heading)        (begin (flush-markdown (string-append content " {.unnumbered .unlisted}")))
                               (flush-verbatim "\n"))
      ((list-item)      (begin (flush-markdown content))
                               (flush-verbatim "\n"))
      ((math-block)     (flush-latex content))
      (else             (flush-markdown content))))
  (flush-output-port))

(define (get-output-string/cache out-port)
  (let ((s (get-output-string out-port #t)))
    (set! cache (cache :append (llm-message "assistant" s)))
    s))

(define (process-streaming-response data out-port)
  (let* ((d (string-trim-both (string-drop data 6)))
         (j (json :parse d)))
    (if (string=? d "[DONE]")
      (begin
        (force-flush-markdown-block! (get-output-string out-port #t))
        (data-end))
      (let* ((content (((j "choices" 0) "delta") :get-string "content" ""))
             (lines ($ content :split "\n")))
        (let ((num-lines (lines :length)))
          (cond
            ((> num-lines 1)
             ;; 处理除最后一行外的完整行
             (lines
              :take (- num-lines 1)
              :for-each
              (lambda (line)
                (display line out-port)
                (flush-markdown-block! (get-output-string/cache out-port))))
             ;; 把最后一行放入新缓冲区
             (display (lines :last) out-port))
            (else
             ;; 不足一行，继续缓冲
             (display content out-port))))))))

(define (llm payload mp)
  (log :debug "Starting streaming llm call...")
  (let* ((callback process-streaming-response)
         (proxy-value (mp 'proxy))
         (req-url (string-append (mp 'base-url) "/chat/completions"))
         (req-data ((payload :to-json mp) :to-string))
         (req-headers (headers (mp 'key)))
         (req-proxy (if (or (null? proxy-value)
                           (and (pair? proxy-value) (null? (car proxy-value))))
                      '()
                      proxy-value)))
    (data-begin)
    ;; init markdown mode
    (display (string-append "markdown:" ""))
    (http-stream-post req-url callback
                      :userdata (open-output-string)
                      :data req-data
                      :headers req-headers
                      :proxy req-proxy)))

(define cache
  (payload #()))

(define (user-chat msg mp)
  (log :debug "Starting user-chat function...")
  (let* ((q (cache :append msg)))
    (log :debug "Added message to cache, about to call streaming llm function...")
    (llm q mp)
    (log :debug "user-chat function completed")))

(define (magic-chat-of-include msg)
  (define (rewrite-path path)
    (if (string-starts? path "~")
        (string-append (if (os-windows?) (getenv "USERPROFILE") (getenv "HOME"))
                       ($ path :strip-prefix "~" :get))
        path))

  (define (include-path raw-path)
    (let1 path (rewrite-path raw-path)
      (if (not (path-exists? path))
          (flush-verbatim (string-append "您指定的文件不存在。文件路径为：" path))
          (begin
            (let* ((content (string-append "文件路径: " path " （从下一行开始是路径对应的文件内容）\n"
                                           (path-read-text path)))
                   (N ($ content :length)))
              (set! cache (cache :append (llm-message "system" content)))
              (flush-verbatim (string-append "纯文本文件已加载：" path "（长度为" (number->string N) "）\n")))))))
  
  (($ msg :split "\n")
   :map string-trim
   :filter (lambda (x) (string-starts? x "%include "))
   :map (lambda (x) ($ x :strip-prefix "%include " :get))
   :for-each include-path))

(define (magic-chat-of-system rest)
  (set! cache (cache :append (llm-message "system" rest)))
  (flush-verbatim (string-append "系统提示词已设置，长度为" (number->string ($ rest :length)))))

(define (magic-chat-of-scheme encoded mp)
  (let* ((scm ($ encoded :strip-prefix "%scheme " :get)))
    (when (log :debug?)
      (log :debug "Starting scheme deserialization, input length: " (number->string (string-length scm)))
      (log :debug "First 200 chars: " (substring scm 0 (min 200 (string-length scm))))
      (log :debug "About to call (read) on large string..."))

    (let ((l (with-input-from-string scm (lambda () (read)))))
      (when (log :debug?)
        (log :debug "Successfully parsed scheme data, list length: " (number->string (length l))))
      
      (log :debug "About to process list elements...")
      (let ((v ($ (list->vector l)
                  :map
                  (lambda (x)
                    (if (string? x)
                        (begin
                          (log :debug "Processing text content")
                          (text-content x))
                        (begin
                          (when (log :debug?)
                            (log :debug "Processing image content with base64 data length: " 
                                          (number->string (string-length (((x 1) 1) 1)))))
                          (log :debug "About to call image-content :from-base64...")
                          (let ((result (image-content :from-base64
                               (((x 1) 1) 1)
                               ((x 1) 2))))
                            (log :debug "Successfully created image content")
                            result))))
                  :collect)))
        (when (log :debug?)
          (log :debug "Created VLM message with " (number->string (vector-length v)) " content elements"))
        (log :debug "About to call user-chat...")
        (let ((message (vlm-message "user" v)))
          (user-chat message mp))))))

(define (magic-chat msg mp)
  (let* ((prefix (($ msg :split " ") 0))
         (rest ($ msg :strip-prefix (string-append prefix " ")
                      :strip-suffix "\n"
                      :get)))
    (cond ((string=? prefix "%system")
           (magic-chat-of-system rest))
          ((string=? prefix "%include")
           (magic-chat-of-include msg))
          ((string=? prefix "%scheme")
           (magic-chat-of-scheme msg mp))
          (else
           (flush-verbatim
            (string-append
             "未找到匹配指令！\n"
             "首行的第一个字符是%，在对话系统中是指令的意思！\n"
             "请查看 帮助->插件->LLM 以获得所有指令的文档"))))))

(define (chat msg mp)
  (if ($ msg :starts-with "%")
      (magic-chat msg mp)
      (begin
        (set! cache (cache :append (llm-message "system" (mp 'default-system))))
        (user-chat (llm-message "user" msg) mp))))

)
)
