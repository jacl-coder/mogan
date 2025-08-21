;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(define-library (liii json)
(import (liii base) (liii lang) (guenchi json))
(export
  json
  json-string-escape json-string-unescape string->json json->string
  json-ref json-ref*
  json-set json-set* json-push json-push* json-drop json-drop* json-reduce json-reduce*)
(begin

(define-class json
  ((data any? #f))

  (define (%get)
    data)

  (define (%get-or-else default)
    (if (%null?)
        default
        data))
  
  (typed-define (%get-string (key any?) (default string?))
    (let1 r (json-ref data key)
      (if (string? r) r default)))
  
  (typed-define (%get-number (key any?) (default number?))
    (let1 r (json-ref data key)
      (if (number? r) r default)))
  
  (define (%keys)
    (if (not (%object?))
        '()
        ((box data) :map car :collect)))
  
  (define (%apply x . xs)
    (@make (apply json-ref* (cons data (cons x xs)))))

  (define (%set x . xs)
    (let ((processed-xs (map (lambda (arg)
                              (if (case-class? arg)
                                  (arg :get)
                                  arg))
                            xs)))
      (json (apply json-set* (cons data (cons x processed-xs))))))
  
  (define (%transform key . args)
    (if (null? args)
        (%this)
        (let ((more-keys ($ args :drop-right 1 :collect))
              (all-args (append (list data key) args)))
          (if (null? more-keys)
              (json (apply json-reduce all-args))
              (json (apply json-reduce* all-args))))))

  (define (%drop key . args)
    (if (null? args)
        (json (json-drop data key))
        (json (apply json-drop* (append (list data key) args)))))

  (define (%push x . xs)
    (let ((processed-xs (map (lambda (arg)
                              (if (case-class? arg)
                                  (arg :get)
                                  arg))
                            xs)))
      (@make (apply json-push* (cons data (cons x processed-xs))))))

  (define (%null?)
    (eq? data 'null))
  
  (define (%object?)
    (and (list? data) (not (null? data))))
  
  (define (%contains-key? key)
    (if (not (%object?))
        #f
        ((box data)
         :exists (lambda (x) (equal? (car x) key)))))
  
  (define (%array?)
    (vector? data))
  
  (define (%string?)
    (string? data))
  
  (define (%number?)
    (number? data))
  
  (define (%integer?)
    (integer? data))
  
  (define (%float?)
    (float? data))
  
  (define (%boolean?)
    (boolean? data))

  
  (define (%to-string)
    (cond ((integer? data) (number->string data))
          ((symbol? data) (symbol->string data))
          ((string? data) data)
          (else (json->string data))))
  
  (chained-define (@null)
    (json 'null))
  
  (chained-define (@true)
    (json 'true))
  
  (chained-define (@false)
    (json 'false))
  
  (chained-define (@parse s)
    (@apply (string->json s)))

  (chained-define (@apply x)
    (let ((j (json)))
      (cond
        ((string? x) (j :set-data! x))
        ((null? x) (j :set-data! 'null))
        ((boolean? x) (if x (j :set-data! 'true) (j :set-data! 'false)))
        ((number? x) (j :set-data! x))
        ((procedure? x)
         (type-error "json: a procedure could not be converted to json case class"))
        (else (j :set-data! x)))
      j))

  (chained-define (@make x)
    (@apply x))
)

) ; end of begin
) ; end of define-library

