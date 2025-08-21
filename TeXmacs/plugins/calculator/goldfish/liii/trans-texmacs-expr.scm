;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(define-library (liii trans-texmacs-expr)

(import (liii list)
        (liii string)
        (liii base))
        
(export preprocess-mogan-scheme
        trans-to-mogan-scheme
        tokenize
        flat->nested
        construct-object
        expression-eval)

(define (preprocess-mogan-scheme scheme-str)
  ; (display scheme-str)
  (with-input-from-string scheme-str
    (lambda () (list (cadr (read))))))

(define (trans-to-mogan-scheme expr)
  (cond 
    ((rational-number :is-type-of expr)
      (let*((express-type (expr :get-express-type)))
            (if (eq? express-type 'decimal)
                (object->string (list 'document (expr :to-decimal-string)))
                (let ((numerator (expr :get-numerator))
                      (denominator (expr :get-denominator)))
                      (if (string=? (denominator :to-string) "1")
                          (object->string (list 'document (numerator :to-string)))
                          (object->string (list 'document `(frac ,(numerator :to-string) ,(denominator :to-string)))))))))
    ((matrix :is-type-of expr)
      (let* ((matrix-list (expr :to-list))
             (texmacs-rows (map (lambda (row)
                                  `(row ,@(map (lambda (cell)
                                                 `(cell ,cell))
                                               row)))
                                matrix-list))
             (texmacs-matrix `(matrix (tformat (table ,@texmacs-rows)))))
        (object->string (list 'document texmacs-matrix))))
    (else (object->string (list 'document "unknown expression type")))))

(define (tokenize lst)
  ; (display* "tokenize: " lst "\n")
  (define (is-math-operator? ch)
    (or (char=? ch #\+)
        (char=? ch #\-)
        (char=? ch #\*)
        (char=? ch #\/)))

  (define (char->symbol char)
    (string->symbol (list->string (list char))))

  (define (tokenize-str acc str last-index index len)
    (if (= index len)
        (if (= last-index index)
            (reverse acc)
            (reverse (cons (substring str last-index index) acc)))
        (let ((str_i (string-ref str index)))
          (if (is-math-operator? str_i)
              (let ((new-acc (if (= last-index index)
                                (cons (char->symbol str_i) acc)
                                (cons (char->symbol str_i)
                                      (cons (substring str last-index index) acc)))))
                (tokenize-str new-acc str (+ index 1) (+ index 1) len))
              (tokenize-str acc str last-index (+ index 1) len)))))

  (define (tokenize-list acc lst)
    (if (null? lst)
        acc
        (let ((element (car lst)))
          (tokenize-list
          (if (string? element)
              (append acc (tokenize-str '() element 0 0 (string-length element)))
              (append acc (list element)))
          (cdr lst)))))

  (tokenize-list '() lst))

;; 定义运算符优先级 (数字越大优先级越高)
(define (operator-precedence op)
  (case op
    ((+ -) 1)
    ((* /) 2)
    ((:transpose ^) 3)
    (else 0)))
  
;; 检查是否为运算符
(define (is-operator? x)
  (and (symbol? x)
        (> (operator-precedence x) 0)))

(define (flat->nested l)
  ; (display* "flat->nested: " l "\n")
  ;; 移除开头的concat（如果存在）
  (define (remove-concat lst)
    (if (and (not (null? lst)) (eq? (car lst) 'concat))
        (cdr lst)
        lst))
  
  ;; 找到最低优先级的运算符位置（找最右边的最低优先级运算符，保证左结合性）
  (define (find-lowest-precedence-op tokens)
    (let loop ((index 0)
               (min-prec 999)
               (min-index #f))
      (if (>= index (length tokens))
          min-index
          (let ((token (list-ref tokens index)))
            (if (is-operator? token)
                (let ((prec (operator-precedence token)))
                  (if (<= prec min-prec)
                      (loop (+ index 1) prec index)
                      (loop (+ index 1) min-prec min-index)))
                (loop (+ index 1) min-prec min-index))))))
  
  ;; 递归构建嵌套结构
  (define (build-nested tokens)
    (cond
      ((null? tokens) '())
      ((= (length tokens) 1) (car tokens))
      ;; 如果没有找到运算符，说明整个表达式是原子的，直接返回
      (else
        (let ((op-index (find-lowest-precedence-op tokens)))
          (if op-index
              ;; 构建 (left-operand operator right-operand)
              (let ((left-tokens (take tokens op-index))
                    (operator (list-ref tokens op-index))
                    (right-tokens (drop tokens (+ op-index 1))))
                (list (build-nested left-tokens)
                      operator
                      (build-nested right-tokens)))
              ;; 没有运算符，整个tokens作为原子表达式返回
              tokens)))))
  
  (let ((cleaned-tokens (remove-concat l)))
    (if (= (length cleaned-tokens) 1)
        cleaned-tokens
        (build-nested cleaned-tokens))))

;; 提取around*结构中的内容
(define (extract-from-around* expr)
  (let ((content (cdr expr)))
    (if (>= (length content) 2)
        (reverse (cdr (reverse (cdr content))))
        content)))

;; 解析 TeXmacs 矩阵表示
(define (parse-texmacs-matrix expr)
  (let* ((tformat-part (cadr expr))
         (table-part (cadr tformat-part))
         (rows-data (cdr table-part))
         (matrix-data (map (lambda (row)
                             (map (lambda (cell)
                                    (cadr cell))
                                  (cdr row)))
                            rows-data)))
    (matrix :form-list matrix-data)))

(define (construct-object expr)
  ; (display* "construct-object: " expr "\n")
  (define (number-string? s)
    (and (string? s)
        (not (eq? (string->number s) #f))))

  (define (dispatch-operator operator-lst)
    (let ((operator-lst-2 (second operator-lst)))
         (if  (string? operator-lst-2)
              (cond 
                ((string=? operator-lst-2 "T") ':transpose)
                ((string=? operator-lst-2 "t") ':transpose)
                ((string=? operator-lst-2 "tr") ':transpose)
                ((number-string? operator-lst-2) `(^ ,(string->number operator-lst-2)))
                (else `(^ ,(expression-eval (flat->nested (tokenize (list operator-lst-2)))))))
              (begin `(^ ,(expression-eval (flat->nested (tokenize (list operator-lst-2)))))))))

  (cond
    ((string? expr) (rational-number expr))
    ((null? expr) (rational-number "0"))
    ((list? expr)
     (let ((type (car expr)))
       (cond 
          ((eq? type 'around*) (expression-eval (flat->nested (tokenize (extract-from-around* expr)))))
          ((eq? type 'frac) 
            (rational-number :frac 
              (expression-eval (flat->nested (tokenize (list (cadr expr))))) 
              (expression-eval (flat->nested (tokenize (cddr expr))))))
          ((eq? type 'concat) (expression-eval (flat->nested (tokenize (cdr expr)))))
          ((eq? type 'matrix) (parse-texmacs-matrix expr))
          ((eq? type 'rsup) (dispatch-operator expr))
          (else (expression-eval expr)))))
    (else expr)))

(define (expression-eval expr)
  ; (display* "expression-eval: " expr "\n")
  (let ((expr-len (length expr)))
    (cond 
      ((= expr-len 1) (construct-object (car expr)))
      ((= expr-len 2) 
        (let ((operator (construct-object (second expr))) 
              (operand (construct-object (car expr))))
              (if (list? operator) 
                  (expression-eval `(,(car expr) ,(car operator) ,(cadr operator)))
                  (operand operator))))
      ((= expr-len 3)
        (let ((operand1 (car expr))
              (operator (cadr expr))
              (operand2 (caddr expr)))
          (let ((eval-op1 (construct-object operand1))
                (eval-op2 (construct-object operand2)))
            (case operator
              ((+) (eval-op1 :+ eval-op2))
              ((-) (eval-op1 :- eval-op2))
              ((*) (eval-op1 :* eval-op2))
              ((/) (eval-op1 :/ eval-op2))
              ((^) (eval-op1 :^ eval-op2))
              (else (error "Unsupported operator or division encountered in expression-eval:" operator))))))
      (else (construct-object expr)))))


)