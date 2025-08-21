;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(define-library (liii markdown)
  (import (liii os) (liii path) (liii uuid) (liii lang) (liii base)
          (liii string))
  (export markdown2latex-via-pandoc

          ;; new streaming tokenizer
          make-stream-tokenizer
          stream-tokenizer-step
          stream-tokenizer-get-blocks
          stream-tokenizer-drain-blocks
          stream-tokenizer-flush
          stream-tokenizer-get-all
          stream-tokenizer-drain-all

          ;; state queries
          stream-tokenizer-complete-blocks
          stream-tokenizer-pending-block)
  (begin

   (define (markdown2latex-via-pandoc text-md pandoc-path)
     (let* ((temp-dir (path :temp-dir :/ "markdown"))
            (temp-name (uuid4))
            (md-temp (temp-dir :/ (string-append temp-name ".md")))
            (latex-temp (temp-dir :/ (string-append temp-name ".latex"))))
       (cond ((not (string? text-md)) (error "Unexpected type of input"))
             (else
               (when (not (file-exists? (temp-dir :to-string)))
                     (mkdir (temp-dir :to-string)))
               (md-temp :write-text text-md)
               (let ((cmd (string-append "\"" pandoc-path "\""
                                         " "
                                         (md-temp :to-string)
                                         " -o "
                                         (latex-temp :to-string)
                                         " -f markdown+tex_math_single_backslash -t latex --no-highlight")))
                 (os-call cmd)
                 (let1 result (latex-temp :read-text)
                   (latex-temp :unlink)
                   ; (md-temp :unlink)
                   result))))))

;;; Streaming Tokenizer for Complete Blocks

;; Define a new stream tokenizer state
   (define-record-type stream-tokenizer
     (make-stream-tokenizer_ complete-blocks pending-lines prev-type current-env)
     stream-tokenizer?
     (complete-blocks stream-tokenizer-complete-blocks set-complete-blocks!)
     (pending-lines stream-tokenizer-pending-lines set-pending-lines!)
     (prev-type stream-tokenizer-prev-type set-prev-type!)
     (current-env stream-tokenizer-current-env set-current-env!))

;; Constructor for empty tokenizer
   (define (make-stream-tokenizer)
     (make-stream-tokenizer_ '() '() #f #f))

;; 行类型判断函数（复用原有逻辑，但稍作简化）
   (define (string-blank? s)
     (string-null? (string-trim-both s char-whitespace?)))

   (define (is-atx-heading? line)
     (let ((trimmed (string-trim line)))
       (and (not (string-blank? trimmed))
            (let loop ((i 0) (count 0))
              (if (and (< i (string-length trimmed))
                       (< count 6)
                       (char=? (string-ref trimmed i) #\#))
                  (loop (+ i 1) (+ count 1))
                  (and (> count 0)
                       (< i (string-length trimmed))
                       (char=? (string-ref trimmed i) #\space)))))))

   (define (is-thematic-break? line)
     (let ((trimmed (string-trim line)))
       (let ((c (if (> (string-length trimmed) 0) (string-ref trimmed 0) #f)))
         (and (or (char=? c #\*) (char=? c #\-) (char=? c #\_))
              (>= (string-length trimmed) 3)
              (let loop ((i 0) (count 0))
                (cond
                  ((= i (string-length trimmed)) (>= count 3))
                  ((char=? (string-ref trimmed i) c)
                   (loop (+ i 1) (+ count 1)))
                  ((or (char=? (string-ref trimmed i) #\space)
                       (char=? (string-ref trimmed i) #\tab))
                   (loop (+ i 1) count))
                  (else #f)))))))

   (define (is-blockquote? line)
     (let ((trimmed (string-trim line)))
       (and (not (string-blank? trimmed))
            (char=? (string-ref trimmed 0) #\>))))

   (define (is-list-item? line current-env)
    (let ((trim (string-trim line)))
      (and (> (string-length trim) 2) ; shortest listitem: `- X`
           (let ((c0 (string-ref trim 0))
                 (c1 (string-ref trim 1)))
                 ;; special rule: leading space line will be treat as list-item
             (or (and (eq? current-env 'list-item)
                      (string-starts? line "   ")) ; 3 spaces
                 (and (or (char=? c0 #\-) (char=? c0 #\+) (char=? c0 #\*))
                      (char=? c1 #\space))
                 (let loop ((i 0) (found-digit #f))
                   (and (< i (string-length trim))
                        (let ((c (string-ref trim i)))
                          (if (and (char>=? c #\0) (char<=? c #\9))
                              (loop (+ i 1) #t)
                              (and found-digit
                                   (or (char=? c #\.) (char=? c #\)))
                                   (< (+ i 1) (string-length trim))
                                   (or (char=? (string-ref trim (+ i 1)) #\space)
                                       ;; special rule: `1.X` where X is not digit will be treat as list-item
                                       ;; for LLM mistake
                                       (char>? (string-ref trim (+ i 1)) #\9))))))))))))


   (define (is-indented-block? line)
     (and (>= (string-length line) 4)
          (string=? (substring line 0 4) "    ")))

   (define (is-fenced-block? line)
     (let ((trimmed (string-trim line)))
       (or (string-starts? trimmed "```")
           (string-starts? trimmed "~~~"))))

   (define (is-table-row? line)
     (let ((trimmed (string-trim line)))
       (and (>= (string-length trimmed) 3)
            (char=? (string-ref trimmed 0) #\|)
            (>= (string-count trimmed #\|) 2))))

;; 判断给定行的类型
   (define (get-line-type line current-env)
     (cond
       ((string-blank? line) 'blank)
       ((is-atx-heading? line) 'heading)
       ((is-thematic-break? line) 'thematic-break)
       ((is-blockquote? line) 'blockquote)
       ((is-list-item? line current-env) 'list-item)
       ((is-indented-block? line) 'indented-block)
       ((is-fenced-block? line) 'fenced-block)
       ((is-table-row? line) 'table)
       (else 'paragraph)))

;; 判断是否需要完成当前块
   (define (should-complete-block prev-type new-type pending-lines)
     (cond
       ;; 如果类型变化，完成当前块
       ((not prev-type) #f)
       ((and (eq? prev-type 'paragraph)
             (eq? new-type 'blank)) #f)
       ((not (eq? prev-type new-type)) #t)
       (else #f)))

;; 完成当前待处理的块
   (define (complete-pending-block tokenizer)
     (let ((pending (stream-tokenizer-pending-lines tokenizer))
           (type (stream-tokenizer-prev-type tokenizer)))
       (if (or (null? pending) (not type))
           tokenizer ; 没有待处理，保持原样
           (let ((new-block (cons type (string-join (reverse pending) "\n")))
                 (complete (stream-tokenizer-complete-blocks tokenizer)))
             (make-stream-tokenizer_
                 (cons new-block complete)
                 '()
                 #f
                 #f)))))

   (define (env-end line-type current-env tokenizer)
     (cond
       ((and (eq? line-type 'fenced-block)
             (eq? current-env 'fenced-block)) #t)
       ((eq? line-type 'fenced-block)
       ;; (not (eq? current-env 'fenced-block))
        (set-current-env! tokenizer line-type)
        #f)
       (else #f)))

;; 向分词器添加一行文本
   (define (stream-tokenizer-step line tokenizer)
     (let* ((prev-type     (stream-tokenizer-prev-type tokenizer))
            (pending-lines (stream-tokenizer-pending-lines tokenizer))
            (current-env   (stream-tokenizer-current-env tokenizer))
            (line-type     (get-line-type line current-env))))
     (cond
       ;; 空行完成非段落块，段落块保留空白行
       ((eq? line-type 'blank)
        (cond
          ((null? pending-lines) tokenizer)  ; 无内容跳过空行
          ((eq? prev-type 'paragraph)
           ;; 把空行作为段落块的一部分
           (make-stream-tokenizer_
               (stream-tokenizer-complete-blocks tokenizer)
               (cons line pending-lines)
               prev-type
               #f))
          (else
           ;; 非段落块遇到空行完成当前块
           (complete-pending-block tokenizer))))

       ;; 处理围栏代码块（当前已经是fenced-block）
       ((eq? current-env 'fenced-block)
        (let ((new-pending (cons line pending-lines)))
          (if (eq? line-type 'fenced-block)
              ;; 结束围栏块
              (let ((new-tokenizer (complete-pending-block
                                    (make-stream-tokenizer_
                                        (stream-tokenizer-complete-blocks tokenizer)
                                        new-pending 'fenced-block #f))))
                (make-stream-tokenizer_
                    (stream-tokenizer-complete-blocks tokenizer)
                    new-pending
                    'fenced-block
                    'fenced-block
                    (stream-tokenizer-complete-blocks new-tokenizer)
                    '() #f #f))
              ;; 继续收集围栏块内容
              (make-stream-tokenizer_
                  (stream-tokenizer-complete-blocks tokenizer)
                  new-pending
                  'fenced-block
                  'fenced-block))))

       ;; 开始新的围栏代码块
       ((eq? line-type 'fenced-block)
        (let ((new-tokenizer (complete-pending-block tokenizer)))
          (make-stream-tokenizer_
              (stream-tokenizer-complete-blocks new-tokenizer)
              (list line)
              'fenced-block
              'fenced-block)))

       ;; 检查是否需要完成当前块并开始新块
       ((or (env-end line-type current-env tokenizer)
            (and (should-complete-block prev-type line-type pending-lines)
                 (not (null? pending-lines))))
        (let ((new-tokenizer (complete-pending-block tokenizer)))
          (make-stream-tokenizer_)))))

;; 获取所有已完成的块（不会返回正在构建的块）
   (define (stream-tokenizer-get-blocks tokenizer)
     (reverse (stream-tokenizer-complete-blocks tokenizer)
              (stream-tokenizer-complete-blocks new-tokenizer)
              (list line)
              (if (eq? line-type 'blank) #f line-type)
              #f
       ;; 继续当前块（合并空行）
       (else))))
 (define (stream-tokenizer-get-blocks tokenizer)
   (reverse (stream-tokenizer-complete-blocks tokenizer)

            line-type))
 (define (stream-tokenizer-drain-blocks tokenizer)
   (let ((blocks (reverse (stream-tokenizer-complete-blocks tokenizer))))
     (set-complete-blocks! tokenizer '())
     blocks))

;; 获取当前正在构建但未完成的块（调试用）
 (define (stream-tokenizer-pending-block tokenizer)
   (let ((pending (stream-tokenizer-pending-lines tokenizer))
         (type (stream-tokenizer-prev-type tokenizer)))
     (if (or (null? pending) (not type))
         #f
         (cons type (string-join (reverse pending) "\n")))))

;; 强制完成当前待处理的块（用于流式处理结束）
 (define (stream-tokenizer-flush tokenizer)
   (complete-pending-block tokenizer))

;; 获取所有内容（包括未完成的块强制完成）
 (define (stream-tokenizer-get-all tokenizer)
   (let ((flushed (stream-tokenizer-flush tokenizer)))
     (stream-tokenizer-get-blocks flushed)))

;; 排出所有内容并清空（包括未完成的块强制完成并清空所有状态）
 (define (stream-tokenizer-drain-all tokenizer)
   (let* ((flushed (stream-tokenizer-flush tokenizer))
          (blocks (stream-tokenizer-drain-blocks flushed)))
     ;; 额外清空所有状态，包括可能的残留
     (set-pending-lines! tokenizer '())
     (set-prev-type! tokenizer #f)
     blocks))

;; 辅助：调试显示状态
 (define (display-tokenizer-state tokenizer)
   (display "=== Stream Tokenizer State ===")
   (newline)
   (display "Complete blocks: ") (display (length (stream-tokenizer-complete-blocks tokenizer))) (newline)
   (display "Pending lines: ") (display (length (stream-tokenizer-pending-lines tokenizer))) (newline)
   (display "Pending type: ") (display (stream-tokenizer-prev-type tokenizer)) (newline)))

