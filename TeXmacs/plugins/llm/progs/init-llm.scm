;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(define-library (session llm)
(import 
  (scheme base)
  (liii string)
  (liii list)
  (liii json)
  (liii os)
  (liii sort)
  (liii logging)
  (liii path))
(export init-llm)
(begin

(use-modules (dynamic session-edit)
             (binary goldfish)
             (binary pandoc))

(set! *load-path*
  (let ((user-path (string-append (getenv "TEXMACS_HOME_PATH") "/plugins/llm/goldfish"))
        (sys-path (string-append (getenv "TEXMACS_PATH") "/plugins/llm/goldfish")))
    (if (file-exists? user-path)
        (cons user-path *load-path*)
        (cons sys-path *load-path*))))

(import (only (liii llm-config) load-llm-menu))

(define log (logging "llm"))
(log :set-path! (path "/tmp/llm.log"))

(define (serialize-as-code lan t)
  (let* ((u (pre-serialize lan t))
         (s (texmacs->code (stree->tree u) "SourceCode")))
    s))

(define (contains-image? elem)
  ;; Check if an element or its children contain an image
  (cond ((not (list? elem)) #f)
        ((null? elem) #f)
        ((eq? (car elem) 'image) #t)
        ((eq? (car elem) 'concat)
         (any contains-image? (cdr elem)))
        (else #f)))

(define (extract-from-concat elem)
  ;; Extarct elements from concat
  (if (and (list? elem) (eq? (car elem) 'concat))
      (cdr elem)
      (list elem))) 

(define (serialize-as-list lan t)
  (let loop ((rest (cdr t)) (current '(document)) (ret '()))
    (cond ((null? rest)
           (if (equal? current '(document))
               ret
               (append ret (list (serialize-latex (texmacs->latex current '()))))))
          ((and (list? (car rest)) (eq? (caar rest) 'image))
           (log :debug "Found image element, processing...")
           (loop (cdr rest)
                 '(document)
                 (append ret
                         (list (serialize-latex (texmacs->latex current '()))
                               (car rest)))))
          ((contains-image? (car rest))
           ;; (log :debug "Found element containing image: " (object->string (car rest)))
           ;; In case of concat
           ;; e.g. (document (concat (image (tuple ...)) "test") "more text")
           (let* ((extracted (extract-from-concat (car rest)))
                  (latex-before (if (equal? current '(document))
                                    ""
                                    (serialize-latex (texmacs->latex current '())))))
             ;; (log :debug "Extracted elements: " (object->string extracted))
             ;; Append extracted elements to the list
             (loop (append extracted (cdr rest))
                   '(document)
                   (if (equal? current '(document)) ret (append ret (list latex-before))))))
          (else
           (loop (cdr rest)
                 (append current (list (car rest)))
                 ret)))))


(define (llm-serialize lan t)
  (if (and (length>=? t 2)
           (eq? (first t) 'document)
           (string? (second t))
           (string-starts? (second t) "%"))
      (string-append (serialize-as-code lan t) "\n<EOF>\n")
      (let* ((l (serialize-as-list lan t)))
        (if (= (length l) 1)
            (string-append (car l) "\n<EOF>\n")
            (begin
              (let* ((original-print-length (*s7* 'print-length))
                     (serialized-string 
                       (begin
                         ;; Fix for large image serialization issue by temporarily setting large print-length
                         (set! (*s7* 'print-length) 922337203685477507)
                         (let ((result (object->string l)))
                           ;; Restore original print-length
                           (set! (*s7* 'print-length) original-print-length)
                           result))))
                (string-append "%scheme " serialized-string "\n<EOF>\n")))))))

(define (llm-launcher)
  (let* ((user "$TEXMACS_HOME_PATH/plugins/llm/goldfish/tm-llm.scm")
         (sys "$TEXMACS_PATH/plugins/llm/goldfish/tm-llm.scm")
         (entry (if (url-exists? user) user sys)))
  (string-append (string-quote (url->system (find-binary-goldfish)))
                 " "
                 (string-quote (url->system entry)))))

(define extra-menus
  (let* ((fallback '("DeepSeek-V3@SiliconFlow"))
         (menu-j (load-llm-menu)))
    (if (menu-j :null?)
        fallback
        (list-stable-sort string<? (menu-j :keys)))))

(define (all-llm-launchers)
  (let ((cmd (llm-launcher)))
    (cons (list :launch (string-append cmd " " "default"))
          (map (lambda (x) (list :launch x (string-append cmd " " x)))
               extra-menus))))

(define (init-llm)
  (plugin-configure llm
    (:require (has-binary-goldfish?))
    (:require (has-binary-pandoc?))
    ,@(all-llm-launchers)
    (:serializer ,llm-serialize))
  
  (when (supports-llm?)
    (for-each
      (lambda (x)
        (session-enable-text-input "llm" x))
      extra-menus)))

;; AI menu integration, keeping old extra-menus
(tm-menu (ai-menu)
  ((verbatim "default") (make-session "llm" "default"))
  (for (model extra-menus)
    (assuming (!= model "default")
      ((eval `(verbatim ,model)) (make-session "llm" model)))))

(menu-bind texmacs-insert-icons
  (former)
  (=> (balloon (icon "tm_ai.xpm") "AI")
       (link ai-menu)))

) ; end of begin
) ; end of define-library

(import (session llm))
(init-llm)
