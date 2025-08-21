
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-calculator.scm
;; DESCRIPTION : Initialize the 'calculator' plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;                   2025  TREE 3
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (dynamic session-edit)
             (binary goldfish))

(import (liii lang))

(define (calculator-serialize lan t)
  (string-append (object->string t)  "\n<EOF>\n"))

(define (goldfish-launcher name)
  (let* ((path ($ "/plugins/" :+ name :+ "/goldfish/tm-" :+ name :+ ".scm" :get))
         (user ($ "$TEXMACS_HOME_PATH" :+ path :get))
         (sys ($ "$TEXMACS_PATH" :+ path :get))
         (entry (if (url-exists? user) user sys)))
  (string-append (string-quote (url->system (find-binary-goldfish)))
                 " -l "
                 (string-quote (url->system entry)))))

(plugin-configure calculator
  (:require (has-binary-goldfish?))
  (:launch ,(goldfish-launcher "calculator"))
  (:serializer ,calculator-serialize)
  (:session "Calculator"))

(when (supports-calculator?)
  (session-enable-math-input "calculator" "default"))