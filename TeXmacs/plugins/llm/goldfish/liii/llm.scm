;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(define-library (liii llm)
(import (liii base) (liii json) (liii http) (texmacs protocol)
        (liii list) (liii os) (liii string) (liii case) (liii lang)
        (liii llm-config) (liii llm-data) (liii llm-chat))
(export model-profile load-llm-key load-llm-menu
        llm-welcome llm-repl)
(begin

(define (validate-api-key key provider)
  (cond 
    ((string=? key "") 
     (flush-verbatim (string-append "\n[Error]: API key for " provider " is empty.\n"
                                   "Please set a valid API key in the ~/.liii_llm_key.json file.\n\n")))
    ((string=? key "Fill in your secret key here") 
     (flush-verbatim (string-append "\n[Error]: You need to replace the default placeholder in the API key field.\n"
                                   "Please replace \"Fill in your secret key here\" with your actual API key.\n\n")))
    (else #t)))

(define (llm-welcome mp version)
  (flush-prompt (string-append (mp 'name) "> "))
  (flush-verbatim (string-append "LLM session " version " by LiiiLabs\n"))
  (flush-verbatim (string-append "Using " (mp 'model) " provided by " (mp 'provider)
                                 " with max_tokens " (number->string (mp 'max-tokens)) "\n"))
  
  ; Validate API key
  (let ((key (mp 'key))
        (provider (mp 'provider)))
    (validate-api-key key provider))
  
  (flush-verbatim "Help docs：Help->Plugins->LLM"))

(define (llm-read-code)
  (define (read-code code)
    (let ((line (read-line)))
      (if (string=? line "<EOF>\n")
          code
          (read-code (append code line)))))
  
    (read-code ""))

(define (eval-and-print code mp)
  (chat code mp))

(define (read-eval-print mp)
  (let ((code (llm-read-code)))
    (if (string=? code "")
        #t
        (eval-and-print code mp))))

(define (llm-repl mp)
  (read-eval-print mp)
  (llm-repl mp))

) ; end of begin
) ; end of define-library

