;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(import (texmacs protocol)
        (liii list)
        (liii string)
        (liii base)
        (liii path)
        (liii lang)
        (liii os))

(set! *load-path*
  (let ((user-path (string-append (getenv "TEXMACS_HOME_PATH") "/plugins/calculator/goldfish"))
        (sys-path (string-append (getenv "TEXMACS_PATH") "/plugins/calculator/goldfish")))
    (if (file-exists? user-path)
        (cons user-path *load-path*)
        (cons sys-path *load-path*))))

(import (liii math-apnum))
(import (liii trans-texmacs-expr))

(define (welcome)
  (flush-prompt "] ")
  (flush-verbatim "Calculator plugin v0.0.1 (still in development, use it with caution)"))

(define (texmacs-str->s-expr texmacs-str)
  (let ((internal-content ($ texmacs-str :replace "<times>" "*" :replace "<ast>" "*" :replace "<div>" "/" :get)))
       (preprocess-mogan-scheme internal-content)))

(define (eval-and-print texmacs-str)
  (let* ((s-expr (texmacs-str->s-expr texmacs-str))
         (eval-result (expression-eval (flat->nested (tokenize s-expr)))))
        (flush-scheme (trans-to-mogan-scheme eval-result))))

(define (read-eval-print)
  (let ((texmacs-str (read-paragraph-by-visible-eof)))
    (if (string=? texmacs-str "")
        #t
        (eval-and-print texmacs-str))))

(define (repl)
  (read-eval-print)
  (repl))

(welcome)
(repl)