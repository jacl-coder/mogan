
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : makrdown.scm
;; DESCRIPTION : markdown data format
;; COPYRIGHT   : (C) 2025  Liii Network
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (data markdown)
  (:use (binary pandoc)))

(import (liii uuid)
        (liii os))

(set! *load-path*
  (let ((user-path (string-append (getenv "TEXMACS_HOME_PATH") "/plugins/markdown/goldfish"))
        (sys-path (string-append (getenv "TEXMACS_PATH") "/plugins/markdown/goldfish")))
    (if (file-exists? user-path)
        (cons user-path *load-path*)
        (cons sys-path *load-path*))))

(import (liii markdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown format defination
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format markdown 
  (:name "Markdown")
  (:suffix "md" "markdown"))

(tm-define (markdown-snippet->latex-snippet text-md)
  (:synopsis "Convert markdown snippet to latex snippet")
  (if (has-binary-pandoc?)
      (markdown2latex-via-pandoc text-md (url->string (find-binary-pandoc)))
      (error "No pandoc binary detected")))

(converter markdown-snippet latex-snippet
  (:function markdown-snippet->latex-snippet))
