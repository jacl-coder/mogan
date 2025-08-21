;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(define-library (liii math-apnum)
(export decimal long-integer rational-number matrix)
(import (liii lang) (liii string) (liii list))
(begin

; apnum是十进制的高精度小数
(define-case-class decimal ()
  ; 高精度数采用向量作为内部表示，假设dlen为1，那么100就是 #(0 0 1)
  (define digits #(0))
  ; 内部表示的每一位的长度，默认值为9，可以保证单节乘法加上单节进位不会溢出
  (define dlen 9)
  ; 默认为0，表示10的零次方
  (define exponent 0)
  ; 符号位，默认为正数
  (define sign '+)

  (define D_FACTOR (expt 10 dlen))

  ; getter方法
  (define (%get-digits) digits)
  (define (%get-dlen) dlen)
  (define (%get-exponent) exponent)
  (define (%get-sign) sign)

  ; setter方法, 只能在静态方法中使用
  (typed-define (%set-digits! (x vector?))
    (set! digits x))

  (typed-define (%set-sign! (x symbol?))
    (set! sign x))

  (typed-define (%set-dlen! (x integer?))
    (when (negative? x)
      (value-error "apnum%set-dlen: x must be positive integer"))
    (set! dlen x))

  (typed-define (%set-exponent! (x integer?))
    (set! exponent x))

  (define (%equals that)
    (%normalize!)
    (if (decimal :is-type-of that)
        (string=? (%to-string) (that :normalize! :to-string))
        (else #f)))

  (define (%copy)
    (let ((c (decimal)))
      (begin
        (c :set-sign! sign)
        (c :set-digits! digits)
        (c :set-exponent! exponent)
        (c :set-dlen! dlen))
      c))

  ; 判断是否为零
  (define (%zero?)
    (let ((len (vector-length digits)))
      (let check-all-zero ((i 0))
        (cond
          ((= i len) #t)  
          ((not (zero? (vector-ref digits i))) #f)  
          (else (check-all-zero (+ i 1)))))))

  (define (%is-integer?)
    (let1 this-copy (%this :copy)  
      (this-copy :normalize!)
      (>= (this-copy :get-exponent) 0)))

  (define (%compare that)
    (let ((diff (%this :- that)))
      (cond
        ((diff :zero?) 0)
        ((eq? (diff :get-sign) '+) 1)
        (else -1))))

  ; 高精度数：0
  (define (@zero)
    (decimal))

  (define (set-self-zero!)
    (set! digits #(0))
    (set! exponent 0)
    (set! sign '+))

  (define (%opposite)
    (let1 d (%copy)  
      (if (eq? (d :get-sign) '+)
        (d :set-sign! '-)
        (d :set-sign! '+))
      d))

  (define (count-trailing-zeros str)
    (let loop ((i (- (string-length str) 1))
              (count 0))
      (if (and (>= i 0)
               (char=? (string-ref str i) #\0))
          (loop (- i 1) (+ count 1))
          count)))

  (define (build-digits-str vec)
    (let ((len (vector-length vec)))
      (if (zero? len)
          ""
          (let ((port (open-output-string)))
            (do ((i (- len 1) (- i 1)))
                ((< i 0))
              (display
              (cond
                ((= i (- len 1))
                  (number->string (vector-ref vec i)))
                (else
                  (let ((s (number->string (vector-ref vec i))))
                    (string-pad s dlen #\0))))
              port))
            (get-output-string port)))))

  (define (build-digits-vector str)
    (let* ((str-len (string-length str)))
      (let process-groups ((start 0) (result-list '()))
        (if (>= start str-len)
            ; 处理完所有分组，设置结果
            (list->vector (reverse result-list))
            ; 处理当前分组
            (let* ((remaining (- str-len start))
                  (group-len (min remaining dlen))
                  (end (+ start group-len))
                  (group-str (substring str 
                                        (- str-len end) 
                                        (- str-len start)))
                  (group-val (string->number group-str)))
              (process-groups end (cons group-val result-list)))))))

  (define (shift-digits-str str n)
    (let ((n (max n (- (count-trailing-zeros str)))))
      (cond
        ((zero? n) str)
        ((> n 0) (string-append str (make-string n #\0)))
        (else
        (let* ((len (string-length str))
                (chars-to-remove (abs n)))
              (substring str 0 (- len chars-to-remove)))))))

  (chained-define (%shift-exponent! n)
    (let* ((digits-str (build-digits-str digits))
           (n (max n (- (count-trailing-zeros digits-str)))))
          (set! exponent (- exponent n))
          (%this :set-digits! (build-digits-vector (shift-digits-str digits-str n))))
    %this)
  
  (chained-define (%normalize!)
    (if (%zero?)
      (set-self-zero!)
        (begin
          ;; 去除digits末尾的零，因为是前导零
          (let* ((trimmed-digits-vec
                  (let loop ((vec digits))
                    (if (and (> (vector-length vec) 1)
                              (zero? (vector-ref vec (- (vector-length vec) 1))))
                        (loop (vector-copy vec 0 (- (vector-length vec) 1)))
                        vec)))
                  (string-representation
                        (build-digits-str trimmed-digits-vec))
                  (num-trailing-zeros (count-trailing-zeros string-representation)))
            (%this :set-digits! trimmed-digits-vec)
            ;; 去除字符串末尾的零
            (%this :shift-exponent! (- num-trailing-zeros)))))
    %this)

  (define (internal-vector-pad-right vec target-len pad-el)
    (let ((orig-len (vector-length vec)))
      (if (>= orig-len target-len)
          vec
          (let* ((padding-len (- target-len orig-len))
                 (padding-vec (make-vector padding-len pad-el)))
            (vector-append vec padding-vec)))))

  (define (internal-compare-digit-vectors vec-a vec-b)
    (let ((len (vector-length vec-a)))
      (let loop ((i (- len 1)))
        (if (< i 0)
            0
            (let ((chunk-a (vector-ref vec-a i))
                  (chunk-b (vector-ref vec-b i)))
              (cond
                ((< chunk-a chunk-b) -1) ; b更大
                ((> chunk-a chunk-b) 1)  ; a更大
                (else (loop (- i 1)))))))))

  (define (internal-subtract-core-vectors v-larger v-smaller)
    (let* ((len (vector-length v-larger))
           (result-vec (make-vector len 0))
           (borrow 0))
      (do ((i 0 (+ i 1)))
          ((= i len)) ; 迭代 [0, len-1]
        (let* ((val1 (vector-ref v-larger i))
               (val2 (vector-ref v-smaller i))
               ;; 从高位借位相当于给当前位减去了 borrow
               (current-diff (- val1 val2 borrow)))
          (if (< current-diff 0)
              (begin ; 需要向更高位借位
                (vector-set! result-vec i (+ current-diff D_FACTOR))
                (set! borrow 1))
              (begin ; 不需要借位
                (vector-set! result-vec i current-diff)
                (set! borrow 0)))))
      result-vec))

  (define (internal-add-core-vectors v1 v2)
    (let* ((len (vector-length v1))
           (result-vec (make-vector len 0))
           (carry 0))
      (do ((i 0 (+ i 1)))
          ((= i len)) ; 迭代 [0, len-1]
        (let* ((sum-chunk (+ (vector-ref v1 i) (vector-ref v2 i) carry)))
          (vector-set! result-vec i (modulo sum-chunk D_FACTOR))
          (set! carry (quotient sum-chunk D_FACTOR))))
      result-vec))

  (define (%+ that)
    (when (%this :zero?) (let ((that-copy ((that :copy) :normalize!))) that-copy))
    (when (that :zero?) (let ((this-copy ((%this :copy) :normalize!))) this-copy))

    ;; 1. 创建副本并在副本上对齐指数
    (let* ((d1-copy (%this :copy))
           (d2-copy (that :copy))
           (exp1 (d1-copy :get-exponent))
           (exp2 (d2-copy :get-exponent))
           (common-exponent (min exp1 exp2)))

      (when (> exp1 common-exponent)
        (d1-copy :shift-exponent! (- exp1 common-exponent)))
      (when (> exp2 common-exponent)
        (d2-copy :shift-exponent! (- exp2 common-exponent)))

      ;; 此刻 d1-copy 和 d2-copy 指数均为 common-exponent

      (let* ((s1 (d1-copy :get-sign))
             (adj-digits1 (d1-copy :get-digits))
             (s2 (d2-copy :get-sign))
             (adj-digits2 (d2-copy :get-digits))

             ;; 2. 扩展向量 (Expand Vectors)
             (len1 (vector-length adj-digits1))
             (len2 (vector-length adj-digits2))
             (target-len (+ (max len1 len2) 1))

             (padded-d1 (internal-vector-pad-right adj-digits1 target-len 0))
             (padded-d2 (internal-vector-pad-right adj-digits2 target-len 0))

             (result-raw-digits #f)
             (final-sign #f))

        ;; 3. 向量加法/减法 (Add/Subtract Vectors)
        (cond
          ((eq? s1 s2)
           (set! result-raw-digits (internal-add-core-vectors padded-d1 padded-d2))
           (set! final-sign s1))
          (else
           (let ((comparison (internal-compare-digit-vectors padded-d1 padded-d2)))
             (cond
               ((= comparison 0)
                (set! result-raw-digits (make-vector target-len 0))
                (set! final-sign '+))
               ((= comparison 1) ; |d1| > |d2|
                (set! result-raw-digits (internal-subtract-core-vectors padded-d1 padded-d2))
                (set! final-sign s1))
               (else ; |d2| > |d1| (comparison = -1)
                (set! result-raw-digits (internal-subtract-core-vectors padded-d2 padded-d1))
                (set! final-sign s2))))))

        ;; 4. 规格化 (Normalize)
        (let ((result-decimal (decimal)))
          (begin
            (result-decimal :set-sign! final-sign)
            (result-decimal :set-digits! result-raw-digits)
            (result-decimal :set-exponent! common-exponent)
            (result-decimal :normalize!))
          result-decimal))))

  (define (%- that)
      (%+ (that :opposite)))

  (define (internal-multiply-core-digits Ad Bd)
    (let* ((lenA (vector-length Ad))
           (lenB (vector-length Bd))
           (lenResult (if (or (zero? lenA) (zero? lenB)) 1 (+ lenA lenB)))
           (result-digits (make-vector lenResult 0)))

      ;; 1. 累加所有 Ai * Bj 到对应的 result-digits[i+j]
      (do ((i 0 (+ i 1))) 
          ((= i lenA))
        (when (not (zero? (vector-ref Ad i)))
          (do ((j 0 (+ j 1)))
              ((= j lenB))
            (let* ((product-chunk (* (vector-ref Ad i) (vector-ref Bd j)))
                   (target-idx (+ i j)))
              (if (< target-idx lenResult)
                  (vector-set! result-digits target-idx
                               (+ (vector-ref result-digits target-idx) product-chunk)))))))
      
      ;; 2. 统一进位传递
      (let ((carry 0))
        (do ((k 0 (+ k 1)))
            ; 迭代到倒数第二个元素
            ((= k (- lenResult 1)))
          (let* ((current-val (+ (vector-ref result-digits k) carry)))
            (vector-set! result-digits k (modulo current-val D_FACTOR))
            (set! carry (quotient current-val D_FACTOR))))
          (vector-set! result-digits (- lenResult 1)
                        (+ (vector-ref result-digits (- lenResult 1)) carry)))
      result-digits))

  (define (%* that)
    (when (or (%this :zero?) (that :zero?)) (decimal))
    (let* ((s1 sign)
           (digits1 digits)
           (exp1 exponent)

           (s2 (that :get-sign))
           (digits2 (that :get-digits))
           (exp2 (that :get-exponent))

           (final-sign (if (eq? s1 s2) '+ '-))
           (final-exponent (+ exp1 exp2))
           (result-raw-digits (internal-multiply-core-digits digits1 digits2)))
      
      ;; 创建新的 decimal 对象
      (let ((result-decimal (decimal)))
        (begin
          (result-decimal :set-sign! final-sign)
          (result-decimal :set-digits! result-raw-digits)
          (result-decimal :set-exponent! final-exponent)
          (result-decimal :normalize!))
        result-decimal)))

  ; 从整数构造高精度数
  (chained-define (@from-integer i)
    (let ((result (decimal)))
      (if (zero? i)
          result
          (begin
            (result :set-sign! (if (< i 0) '- '+))
            (let* ((val (abs i))
                   (div (expt 10 (result :get-dlen))))
              (let collect-digits ((num val) (digits-list '()))
                (if (zero? num)
                    (let ((final-list (if (null? digits-list)
                                        '(0)
                                        digits-list)))
                      (result :set-digits! (list->vector (reverse final-list))))
                    (collect-digits (quotient num div)
                                    (cons (remainder num div) digits-list)))))))
          result))
  
  ; 从字符串构造高精度数
  (typed-define (@apply (num-str string?))
    (when (string-null? num-str) (value-error "decimal@apply: empty string is not allowed"))

    (define has-sign? (or (char=? (string-ref num-str 0) #\+) 
                          (char=? (string-ref num-str 0) #\-)))

    (let ((num-str-drop-head num-str) (len (string-length num-str)))
      (when has-sign?
        (set! num-str-drop-head ($ num-str :drop 1 :get))
        (set! len (- len 1)))
      (when (string-null? num-str-drop-head) (value-error "decimal@apply: invalid number string: " num-str))

      (when (not ($ num-str-drop-head :forall (lambda (x) (or (x :digit?) (x :equals #\.)))))
        (value-error "decimal@apply: numbers can only contain numbers and dot.  get: " num-str))

      (let  ((cnt ($ num-str-drop-head :count (lambda (x) (x :equals #\.))))
             (index ($ num-str-drop-head :index-of #\.)))
        (when (or (> cnt 1) 
                  (or (= index 0) (= index (- len 1))))
              (value-error "decimal@apply: The number of dots cannot be greater than 1, 
                            and dots cannot be placed at the beginning and end of the number.  get: " num-str))))

    (let  ((result (decimal)))
          (begin
            ; 处理符号位
            (let* ((first-char (string-ref num-str 0))
                  (num-str-without-sign (if has-sign?
                                          (substring num-str 1 (string-length num-str))
                                          num-str))
                  (sign-value (if (and has-sign? (char=? first-char #\-)) '- '+)))
              (result :set-sign! sign-value)
              
              (define (result-set-exponent! has-decimal-point?)
                (when has-decimal-point?
                  (let* ((decimal-point-pos ($ num-str-without-sign :index-of #\.))
                          (frac-len (- (string-length num-str-without-sign) 
                                    decimal-point-pos 
                                    1)))
                    (result :set-exponent! (- 0 frac-len)))))

              ; 查找小数点位置并处理
              (let* ((has-decimal-point? ($ num-str-without-sign :contains "."))
                      (set-exponent (result-set-exponent! has-decimal-point?))
                      (num-str-without-point ($ num-str-without-sign :replace-first "." "" :get))
                      (dlen (result :get-dlen)))
                
                ; 构建数字向量（从低位到高位）
                (let* ((str-len (string-length num-str-without-point)))
                  (let process-groups ((start 0) (result-list '()))
                    (if (>= start str-len)
                        ; 处理完所有分组，设置结果
                        (result :set-digits! (list->vector (reverse result-list)))
                        ; 处理当前分组
                        (let* ((remaining (- str-len start))
                              (group-len (min remaining dlen))
                              (end (+ start group-len))
                              (group-str (substring num-str-without-point 
                                                    (- str-len end) 
                                                    (- str-len start)))
                              (group-val (string->number group-str)))
                          (process-groups end (cons group-val result-list)))))))))
      result))
 
  (define (remove-leading-zeros str)
    (let ((len (string-length str)))
      (if (zero? len)
          "0"
          (let loop ((i 0))
            (cond
              ((>= i len) "0")
              ((char=? (string-ref str i) #\0)
              (loop (+ i 1)))
              (else (substring str i len)))))))

  ;; 高精度数的字符串表示
  (define (%to-string)
    (let ((result-str ""))
      ;; 符号处理
      (when (eq? sign '-)
        (set! result-str (string-append result-str "-")))
      
      (let* ((digits-str (remove-leading-zeros (build-digits-str digits))))                  
        ;; 处理指数（决定小数点位置）
        (cond 
          ((= exponent 0)
           (string-append result-str digits-str))
          ((> exponent 0)
           (string-append result-str digits-str (make-string exponent #\0)))
          (else 
           (let ((abs-exp (abs exponent)))
             (if (<= abs-exp (string-length digits-str))
                 ;; 小数点在数字内部
                 (let* ((int-end (- (string-length digits-str) abs-exp))
                        (int-part (if (zero? int-end) "0" (substring digits-str 0 int-end)))
                        (frac-part (substring digits-str int-end)))
                   (string-append result-str int-part "." frac-part))
                 ;; 需要多个前导零
                 (string-append result-str "0." 
                                (make-string (- abs-exp (string-length digits-str)) #\0)
                                digits-str))))))))
  
) ; end of decimal

(define-case-class long-integer ()
  (define digits #(0))
  (define dlen 9)
  (define sign '+)
  (define D_FACTOR (expt 10 dlen)) 

  (define (%get-digits) digits)
  (define (%get-dlen) dlen)
  (define (%get-sign) sign)

  (typed-define (%set-digits! (x vector?))
    (set! digits x))

  (typed-define (%set-sign! (x symbol?))
    (set! sign x))

  (typed-define (%set-dlen! (x integer?))
    (when (negative? x)
      (value-error "long-integer%set-dlen: x must be positive integer"))
    (set! dlen x))

  (define (%equals that)
    (if (long-integer :is-type-of that)
        (string=? (%to-string) (that :to-string))
        (else #f)))
  
  (define (%copy)
    (let ((c (long-integer)))
      (begin
        (c :set-sign! sign)
        (c :set-digits! digits)
        (c :set-dlen! dlen))
      c))
  
  (define (%zero?)
    (let ((len (vector-length digits)))
      (let check-all-zero ((i 0))
        (cond
          ((= i len) #t)  
          ((not (zero? (vector-ref digits i))) #f)  
          (else (check-all-zero (+ i 1)))))))

  (define (@zero)
    (long-integer))
  
  (define (set-self-zero!)
    (set! digits #(0))
    (set! sign '+))
  
  (define (%opposite)
    (let1 d (%copy)  
      (if (eq? (d :get-sign) '+)
        (d :set-sign! '-)
        (d :set-sign! '+))
      d))
  
  (define (%abs)
    (let1 d (%copy)
      (when (eq? (d :get-sign) '-)
        (d :set-sign! '+))
      d))

  (chained-define (%normalize!)
    (if (%zero?)
        (set-self-zero!)
        (let ((current-digits digits)
              (len (vector-length digits)))
          (if (> len 1)
              (let find-first-non-zero-msb ((idx (- len 1)))
                (cond
                  ((not (zero? (vector-ref current-digits idx)))
                   (if (< idx (- len 1))
                       (set! digits (vector-copy current-digits 0 (+ idx 1)))))
                  (else
                   (find-first-non-zero-msb (- idx 1))))))))
    %this)

  (define (remove-leading-zeros str)
    (let ((len (string-length str)))
      (if (zero? len)
          "0"
          (let loop ((i 0))
            (cond
              ((>= i len) "0")
              ((char=? (string-ref str i) #\0)
               (loop (+ i 1)))
              (else (substring str i len)))))))

  (define (build-digits-str vec)
    (let ((len (vector-length vec)))
      (if (zero? len)
          ""
          (let ((port (open-output-string)))
            (do ((i (- len 1) (- i 1)))
                ((< i 0))
              (display
              (cond
                ((= i (- len 1))
                  (number->string (vector-ref vec i)))
                (else
                  (let ((s (number->string (vector-ref vec i))))
                    (string-pad s dlen #\0))))
              port))
            (get-output-string port)))))

  (define (build-digits-vector str)
    (let* ((str-len (string-length str)))
      (let process-groups ((start 0) (result-list '()))
        (if (>= start str-len)
            ; 处理完所有分组，设置结果
            (list->vector (reverse result-list))
            ; 处理当前分组
            (let* ((remaining (- str-len start))
                  (group-len (min remaining dlen))
                  (end (+ start group-len))
                  (group-str (substring str 
                                        (- str-len end) 
                                        (- str-len start)))
                  (group-val (string->number group-str)))
              (process-groups end (cons group-val result-list)))))))

  (define (internal-vector-pad-right vec target-len pad-el)
    (let ((orig-len (vector-length vec)))
      (if (>= orig-len target-len)
          vec
          (let* ((padding-len (- target-len orig-len))
                 (padding-vec (make-vector padding-len pad-el)))
            (vector-append vec padding-vec)))))

  (define (internal-compare-digit-vectors vec-a vec-b)
    (let ((len (vector-length vec-a)))
      (let loop ((i (- len 1)))
        (if (< i 0)
            0
            (let ((chunk-a (vector-ref vec-a i))
                  (chunk-b (vector-ref vec-b i)))
              (cond
                ((< chunk-a chunk-b) -1) ; b更大
                ((> chunk-a chunk-b) 1)  ; a更大
                (else (loop (- i 1)))))))))

  (define (internal-subtract-core-vectors v-larger v-smaller)
    (let* ((len (vector-length v-larger))
           (result-vec (make-vector len 0))
           (borrow 0))
      (do ((i 0 (+ i 1)))
          ((= i len)) ; 迭代 [0, len-1]
        (let* ((val1 (vector-ref v-larger i))
               (val2 (vector-ref v-smaller i))
               ;; 从高位借位相当于给当前位减去了 borrow
               (current-diff (- val1 val2 borrow)))
          (if (< current-diff 0)
              (begin ; 需要向更高位借位
                (vector-set! result-vec i (+ current-diff D_FACTOR))
                (set! borrow 1))
              (begin ; 不需要借位
                (vector-set! result-vec i current-diff)
                (set! borrow 0)))))
      result-vec))

  (define (internal-add-core-vectors v1 v2)
    (let* ((len (vector-length v1))
           (result-vec (make-vector len 0))
           (carry 0))
      (do ((i 0 (+ i 1)))
          ((= i len)) ; 迭代 [0, len-1]
        (let* ((sum-chunk (+ (vector-ref v1 i) (vector-ref v2 i) carry)))
          (vector-set! result-vec i (modulo sum-chunk D_FACTOR))
          (set! carry (quotient sum-chunk D_FACTOR))))
      result-vec))
  
  (define (%+ that)
    (when ((%this) :zero?) (let ((that-copy (that :copy))) that-copy))
    (when (that :zero?) (let ((this-copy (%this :copy))) this-copy))

    ;; 1. 创建副本并在副本上对齐指数
    (let* ((d1-copy ((%this :copy) :normalize!))
           (d2-copy ((that :copy) :normalize!)))

      (let* ((s1 (d1-copy :get-sign))
             (adj-digits1 (d1-copy :get-digits))
             (s2 (d2-copy :get-sign))
             (adj-digits2 (d2-copy :get-digits))

             ;; 2. 扩展向量 (Expand Vectors)
             (len1 (vector-length adj-digits1))
             (len2 (vector-length adj-digits2))
             (target-len (+ (max len1 len2) 1))

             (padded-d1 (internal-vector-pad-right adj-digits1 target-len 0))
             (padded-d2 (internal-vector-pad-right adj-digits2 target-len 0))

             (result-raw-digits #f)
             (final-sign #f))

        ;; 3. 向量加法/减法 (Add/Subtract Vectors)
        (cond
          ((eq? s1 s2)
           (set! result-raw-digits (internal-add-core-vectors padded-d1 padded-d2))
           (set! final-sign s1))
          (else
           (let ((comparison (internal-compare-digit-vectors padded-d1 padded-d2)))
             (cond
               ((= comparison 0)
                (set! result-raw-digits (make-vector target-len 0))
                (set! final-sign '+))
               ((= comparison 1) ; |d1| > |d2|
                (set! result-raw-digits (internal-subtract-core-vectors padded-d1 padded-d2))
                (set! final-sign s1))
               (else ; |d2| > |d1| (comparison = -1)
                (set! result-raw-digits (internal-subtract-core-vectors padded-d2 padded-d1))
                (set! final-sign s2))))))

        (let ((result-long-integer (long-integer)))
          (begin
            (result-long-integer :set-sign! final-sign)
            (result-long-integer :set-digits! result-raw-digits)
            (result-long-integer :normalize!))
          result-long-integer))))

  (define (%- that)
      (%+ (that :opposite)))

  (define (internal-multiply-core-digits Ad Bd)
    (let* ((lenA (vector-length Ad))
           (lenB (vector-length Bd))
           (lenResult (if (or (zero? lenA) (zero? lenB)) 1 (+ lenA lenB)))
           (result-digits (make-vector lenResult 0)))

      ;; 1. 累加所有 Ai * Bj 到对应的 result-digits[i+j]
      (do ((i 0 (+ i 1))) 
          ((= i lenA))
        (when (not (zero? (vector-ref Ad i)))
          (do ((j 0 (+ j 1)))
              ((= j lenB))
            (let* ((product-chunk (* (vector-ref Ad i) (vector-ref Bd j)))
                   (target-idx (+ i j)))
              (if (< target-idx lenResult)
                  (vector-set! result-digits target-idx
                               (+ (vector-ref result-digits target-idx) product-chunk)))))))
      
      ;; 2. 统一进位传递
      (let ((carry 0))
        (do ((k 0 (+ k 1)))
            ; 迭代到倒数第二个元素
            ((= k (- lenResult 1)))
          (let* ((current-val (+ (vector-ref result-digits k) carry)))
            (vector-set! result-digits k (modulo current-val D_FACTOR))
            (set! carry (quotient current-val D_FACTOR))))
          (vector-set! result-digits (- lenResult 1)
                        (+ (vector-ref result-digits (- lenResult 1)) carry)))
      result-digits))

  (define (%* that)
    (when (or (%this :zero?) (that :zero?)) (long-integer))
    (let* ((s1 sign)
           (digits1 digits)

           (s2 (that :get-sign))
           (digits2 (that :get-digits))

           (final-sign (if (eq? s1 s2) '+ '-))
           (result-raw-digits (internal-multiply-core-digits digits1 digits2)))
      
      (let ((result-long-integer (long-integer)))
        (begin
          (result-long-integer :set-sign! final-sign)
          (result-long-integer :set-digits! result-raw-digits)
          (result-long-integer :normalize!))
        result-long-integer)))

        
   ;; 整数的非负整数幂
  (define (%^ n)
    (unless (integer? n)
      (value-error "long-integer%^: exponent must be an integer" n))
    
    (unless (>= n 0)
      (value-error "long-integer%^: exponent must be a non-negative integer" n))
    
    (cond
      ;; 0^0 情况校验
      ((and (= n 0) (%zero?)) 
       (value-error "long-integer%^: 0^0 is undefined"))
      ;; 0次幂返回1（仅当底数非0时）
      ((= n 0) (long-integer "1"))
      ;; 底数为0且指数>0时返回0
      ((%zero?) (long-integer "0"))
      ;; 1次幂直接返回底数副本
      ((= n 1) (%copy))
      ;; 常规情况：使用快速幂算法
      (else      
       (let ((result (long-integer "1"))
             (base (%copy))
             (exponent n))
         (let loop ((result-acc result) (base-acc base) (exp exponent))
           (cond
             ((= exp 0) result-acc)
             ((odd? exp)
              (loop (result-acc :* base-acc) 
                    base-acc 
                    (- exp 1)))
             (else
              (loop result-acc 
                    (base-acc :* base-acc) 
                    (quotient exp 2)))))))))


  (define (%compare-abs other-long-int)
    (let ((self-abs-copy (%this :copy))
          (other-abs-copy (other-long-int :copy)))
      (let* ((s-digits (self-abs-copy :get-digits))
             (o-digits (other-abs-copy :get-digits))
             (s-len (vector-length s-digits))
             (o-len (vector-length o-digits)))
        (cond
          ((> s-len o-len) 1)
          ((< s-len o-len) -1)
          (else ; 长度相等
           (let loop ((idx (- s-len 1))) 
             (if (< idx 0)
                 0 ; 所有块都相等
                 (let ((s-chunk (vector-ref s-digits idx))
                       (o-chunk (vector-ref o-digits idx)))
                   (cond
                     ((> s-chunk o-chunk) 1)
                     ((< s-chunk o-chunk) -1)
                     (else (loop (- idx 1))))))))))))

  (define (%div-rem that)
    (let ((this-copy ((%this :copy) :normalize!))
          (that-copy ((that :copy) :normalize!)))
      
      (when (that-copy :zero?)
        (value-error "long-integer:%div-rem: division by zero" that))

      (let*  ((sign-a (this-copy :get-sign))
              (sign-b (that-copy :get-sign))
              (abs-a (this-copy :abs))
              (abs-b (that-copy :abs))
              (quotient-sign (if (eq? sign-a sign-b) '+ '-))
              (remainder-sign sign-a)) 

        (if (< (abs-a :compare-abs abs-b) 0)
            (let ((q (long-integer "0"))
                  (r (this-copy :copy)))
              (q :set-sign! '+) 
              (list q r))
            (let*((Q-val (long-integer "0")) 
                  (R-val (abs-a :copy)) 
                  (len-R-initial-str (string-length (R-val :to-string)))
                  (len-abs-b-str (string-length (abs-b :to-string)))
                  (max-pos (- len-R-initial-str len-abs-b-str))) 
              
              (do ((pos max-pos (- pos 1)))
                  ((< pos 0)) 
                (when (>= pos 0) 
                  (let* ((factor-str (string-append "1" (make-string pos #\0)))
                          (factor (long-integer factor-str)) 
                          (divisor-scaled (abs-b :* factor))) 
                    (let loop ((count 0))
                      (let ((comparison-result (R-val :compare-abs divisor-scaled)))
                        (when (>= comparison-result 0)
                          (set! R-val (R-val :- divisor-scaled))
                          (set! Q-val (Q-val :+ factor))
                          (loop (+ count 1))))))))
              
              (Q-val :set-sign! quotient-sign)
              (when (and (Q-val :zero?) (eq? quotient-sign '-)) (Q-val :set-sign! '+))
              
              (R-val :set-sign! remainder-sign) 
              (when (and (R-val :zero?) (eq? remainder-sign '-)) (R-val :set-sign! '+))
              
              (list Q-val R-val))))))

  (define (%/ that)
    (car ((%this :copy) :div-rem that)))

  (define (%% that)
    (cadr ((%this :copy) :div-rem that)))

  (define (@gcd a b)
    (if (b :zero?)
        (a :abs)
        (@gcd b (a :% b))))


  (chained-define (@from-integer i)
    (let ((result (long-integer)))
      (if (zero? i)
          result
          (begin
            (result :set-sign! (if (< i 0) '- '+))
            (let* ((val (abs i))
                   (div (expt 10 (result :get-dlen))))
              (let collect-digits ((num val) (digits-list '()))
                (if (zero? num)
                    (let ((final-list (if (null? digits-list)
                                        '(0)
                                        digits-list)))
                      (result :set-digits! (list->vector (reverse final-list))))
                    (collect-digits (quotient num div)
                                    (cons (remainder num div) digits-list)))))))
      (result :normalize!)
      result))

  (typed-define (@apply (num-str string?))
    (when (string-null? num-str)
      (value-error "long-integer@apply: empty string is not allowed" num-str))

    (let ((result (long-integer)))      
      (let* ((len (string-length num-str))
             (dlen (result :get-dlen))
             (first-char (string-ref num-str 0))
             (has-sign (and (> len 0)
                            (or (char=? first-char #\+)
                                (char=? first-char #\-))))
             (actual-sign (if (and has-sign (char=? first-char #\-)) '- '+))
             (digits-part (if has-sign
                              (if (= len 1) "" (substring num-str 1 len))
                              num-str)))

        (when (string-null? digits-part)
          (value-error "long-integer@apply: number string contains only a sign or is invalid" num-str))

        (result :set-sign! actual-sign)

        (let* ((str-len (string-length digits-part)))
              (let process-groups ((start 0) (result-list '()))
                (if (>= start str-len)
                    ; 处理完所有分组，设置结果
                    (result :set-digits! (list->vector (reverse result-list)))
                    ; 处理当前分组
                    (let* ((remaining (- str-len start))
                          (group-len (min remaining dlen))
                          (end (+ start group-len))
                          (group-str (substring digits-part 
                                                (- str-len end) 
                                                (- str-len start)))
                          (group-val (string->number group-str)))
                      (process-groups end (cons group-val result-list))))))  
        
        (result :normalize!)
        result)))

  (define (%to-string)
    (if (%this :zero?)
        "0"
        (let* ((sign-str (if (eq? (%this :get-sign) '-) "-" ""))
               (digits-val (%this :get-digits))
               (raw-num-str (build-digits-str digits-val)) 
               (num-str (remove-leading-zeros raw-num-str)))
          (string-append sign-str num-str))))
  
) ; end of long-integer

(define-case-class rational-number ()
  (define numerator (long-integer))
  (define denominator (long-integer "1"))
  (define express-type 'decimal)
  (define significant-figures-num 50)
  (define decimal-precision 50)

  (define (%get-numerator) numerator)
  (define (%get-denominator) denominator)
  (define (%get-express-type) express-type)
  (define (%get-significant-figures-num) significant-figures-num)
  (define (%get-decimal-precision) decimal-precision)

  (define (%set-numerator! x)
    (unless (long-integer :is-type-of x)
      (value-error "rational-number%set-numerator!: x must be long-integer" x))
    (set! numerator x))

  (define (%set-denominator! x)
    (unless (long-integer :is-type-of x)
      (value-error "rational-number%set-denominator!: x must be long-integer" x))
    (unless (not (x :zero?))
      (value-error "rational-number%set-denominator!: denominator cannot be zero" x))
    (set! denominator x))
  
  (define (%set-express-type! x)
    (unless (or (eq? x 'rational) (eq? x 'decimal))
      (value-error "rational-number%set-express-type!: x must be rational or decimal" x))
    (set! express-type x))

  (define (%set-significant-figures-num! x)
    (unless (integer? x)
      (value-error "rational-number%set-significant-figures-num!: x must be integer" x))
    (set! significant-figures-num x))

  (define (%set-decimal-precision! x)
    (unless (integer? x)
      (value-error "rational-number%set-decimal-precision!: x must be integer" x))
    (set! decimal-precision x))
  
  (chained-define (%normalize!)
    (let ((gcd-num (long-integer :gcd numerator denominator)))
      (begin 
        (set! numerator (numerator :/ gcd-num))
        (set! denominator (denominator :/ gcd-num))
        (if (eq? (numerator :get-sign) (denominator :get-sign))
            (begin (numerator :set-sign! '+) (denominator :set-sign! '+))
            (begin (numerator :set-sign! '-) (denominator :set-sign! '+))))
      %this))

  (define (%copy)
    (let ((c (rational-number)))
      (begin
        (c :set-numerator! numerator)
        (c :set-denominator! denominator)
        (c :set-express-type! express-type)
        (c :set-significant-figures-num! significant-figures-num)
        (c :set-decimal-precision! decimal-precision))
      c))

  (define (%equals that)
    (%normalize!)
    (and (rational-number :is-type-of that) 
         (string=? (%to-string) (that :normalize! :to-string))))
  
  (define (%+ that)
    (let*((this-copy ((%this :copy) :normalize!))
          (that-copy ((that :copy) :normalize!))
          (this-type (this-copy :get-express-type))
          (that-type (that-copy :get-express-type))
          (fa (this-copy :get-numerator))
          (fb (this-copy :get-denominator))
          (fc (that-copy :get-numerator))
          (fd (that-copy :get-denominator))
          (result (rational-number)))
          
          (cond 
            ((fa :zero?) (result :set-numerator! fc) (result :set-denominator! fd))
            ((fc :zero?) (result :set-numerator! fa) (result :set-denominator! fb))
            ((fb :equals fd) (result :set-numerator! (fa :+ fc)) (result :set-denominator! fb))
            (else           
              (result :set-numerator! ((fa :* fd) :+ (fb :* fc)))
              (result :set-denominator! (fb :* fd))))

          (when (or (eq? this-type 'rational) 
                    (eq? that-type 'rational))
            (result :set-express-type! 'rational))

          (result :normalize!)
          result))

  (define (%- that)
    (let*((this-copy ((%this :copy) :normalize!))
          (that-copy ((that :copy) :normalize!))
          (this-type (this-copy :get-express-type))
          (that-type (that-copy :get-express-type))
          (fa (this-copy :get-numerator))
          (fb (this-copy :get-denominator))
          (fc (that-copy :get-numerator))
          (fd (that-copy :get-denominator))
          (result (rational-number)))
          
          (cond 
            ((fa :zero?) (result :set-numerator! fc) (result :set-denominator! (fd :opposite)))
            ((fc :zero?) (result :set-numerator! fa) (result :set-denominator! (fb :opposite)))
            ((fb :equals fd) (result :set-numerator! (fa :- fc)) (result :set-denominator! fb))
            (else           
              (result :set-numerator! ((fa :* fd) :- (fb :* fc)))
              (result :set-denominator! (fb :* fd))))
          
          (when (or (eq? this-type 'rational) 
                    (eq? that-type 'rational))
            (result :set-express-type! 'rational))

          (result :normalize!)
          result))

  (define (%* that)
    (let*((this-copy ((%this :copy) :normalize!))
          (that-copy ((that :copy) :normalize!))
          (this-type (this-copy :get-express-type))
          (that-type (that-copy :get-express-type))
          (fa (this-copy :get-numerator))
          (fb (this-copy :get-denominator))
          (fc (that-copy :get-numerator))
          (fd (that-copy :get-denominator))
          (result (rational-number)))
          (cond
            ((or (fa :zero?) (fc :zero?)) (result :set-numerator! (long-integer :zero)))
            ((fa :equals fd) (result :set-numerator! fc) (result :set-denominator! fb))
            ((fb :equals fc) (result :set-numerator! fa) (result :set-denominator! fd))
            (else 
              (result :set-numerator! (fa :* fc))
              (result :set-denominator! (fb :* fd))))
          
          (when (or (eq? this-type 'rational) 
                    (eq? that-type 'rational))
            (result :set-express-type! 'rational))

          (result :normalize!)
          result))

  (define (%/ that)
    (let*((this-copy ((%this :copy) :normalize!))
          (that-copy ((that :copy) :normalize!))
          (this-type (this-copy :get-express-type))
          (that-type (that-copy :get-express-type))
          (fa (this-copy :get-numerator))
          (fb (this-copy :get-denominator))
          (fc (that-copy :get-numerator))
          (fd (that-copy :get-denominator))
          (result (rational-number)))
          
          (when (fc :zero?)
            (value-error "rational-number%/: division by zero"))

          (cond
            ((fa :zero?) (result :set-numerator! (long-integer :zero)))
            ((fa :equals fc) (result :set-numerator! fd) (result :set-denominator! fb))
            ((fb :equals fd) (result :set-numerator! fa) (result :set-denominator! fc))
            (else 
              (result :set-numerator! (fa :* fd))
              (result :set-denominator! (fb :* fc))))

          (when (or (eq? this-type 'rational) 
                    (eq? that-type 'rational))
            (result :set-express-type! 'rational))

          (result :normalize!)
          result))

  ;; 有理数的整数幂运算（支持负指数）
  (define (%^ n)
    (when (rational-number :is-type-of n) (set! n (string->number (n :to-decimal-string))))
    (unless (integer? n)
      (value-error "rational-number%^: exponent must be an integer" n))

    (let ((this-copy (%this :normalize! :copy)))
      (cond
        ;; 0^0 情况校验
        ((and (= n 0) ((this-copy :get-numerator) :zero?)) 
         (value-error "rational-number%^: 0^0 is undefined"))
        ((and (< n 0) ((this-copy :get-numerator) :zero?))
         (value-error "rational-number%^: 0^negative_power is undefined"))
        ((= n 0) (rational-number "1"))
        (((this-copy :get-numerator) :zero?) (rational-number "0"))
        ((= n 1) this-copy)
        ;; 负指数：
        ((< n 0)
         (let* ((num (this-copy :get-numerator))
                (den (this-copy :get-denominator))
                (abs-n (abs n))
                (result (rational-number)))
           (result :set-numerator! (den :^ abs-n))
           (result :set-denominator! (num :^ abs-n))
           (result :set-express-type! (this-copy :get-express-type))
           (result :normalize!)
           result))
        (else      
         (let* ((num (this-copy :get-numerator))
                (den (this-copy :get-denominator))
                (result (rational-number)))
           (result :set-numerator! (num :^ n))
           (result :set-denominator! (den :^ n))
           (result :set-express-type! (this-copy :get-express-type))
           (result :normalize!)
           result)))))

  (chained-define (@frac a b)
    (unless (or (long-integer :is-type-of a) (integer? a) (rational-number :is-type-of a))
      (value-error "rational-number@frac: a must be a integer , long-integer or rational-number " a))

    (unless (or (long-integer :is-type-of b) (integer? b) (rational-number :is-type-of b))
      (value-error "rational-number@frac: b must be a integer , long-integer or rational-number " b))
    
    (let ((result (rational-number)))
      (begin
        (cond
          ((and (rational-number :is-type-of a) (rational-number :is-type-of b))
            (let  ((tmp (a :/ b)))
                  (result :set-numerator! (tmp :get-numerator))
                  (result :set-denominator! (tmp :get-denominator))))
          ((and (rational-number :is-type-of b) (not (rational-number :is-type-of a)))
            (let  ((tmp ((rational-number :frac (if (long-integer :is-type-of a) a (long-integer :from-integer a)) 1) :/ b)))
                  (result :set-numerator! (tmp :get-numerator))
                  (result :set-denominator! (tmp :get-denominator)))
          )
          ((and (rational-number :is-type-of a) (not (rational-number :is-type-of b)))
            (let  ((tmp (a :/ (rational-number :frac (if (long-integer :is-type-of b) b (long-integer :from-integer b)) 1))))
                  (result :set-numerator! (tmp :get-numerator))
                  (result :set-denominator! (tmp :get-denominator)))
          )
          (else 
            (result :set-numerator! (if (long-integer :is-type-of a) a (long-integer :from-integer a)))
            (result :set-denominator! (if (long-integer :is-type-of b) b (long-integer :from-integer b)))))
        (result :set-express-type! 'rational)
        (result :normalize!))
      result))

  (define (%to-string)
    (if (numerator :zero?)
        "0"
        (if (string=? (denominator :to-string) "1") 
            (numerator :to-string)
            (string-append (numerator :to-string) "/" (denominator :to-string)))))

  (typed-define (@apply (num-str string?))
    (when (string-null? num-str)
      (value-error "rational-number@apply: empty string is not allowed" num-str))

    (let ((new-rational (rational-number)))
      (let ((slash-index ($ num-str :index-of #\/)))
        (cond
          ;; 分数
          ((and (integer? slash-index) (>= slash-index 0))
           (when (or (= slash-index 0)
                     (= slash-index (- (string-length num-str) 1))
                     (> ($ num-str :count (lambda (c) (c :equals #\/))) 1))
             (value-error "rational-number@apply: invalid fraction format '" num-str "' (slash position or multiple slashes)"))

           (let ((num-part (substring num-str 0 slash-index))
                 (den-part (substring num-str (+ slash-index 1) (string-length num-str))))
                (when (or (string-null? num-part) (string-null? den-part))
                  (value-error "rational-number@apply: empty numerator or denominator in fraction '" num-str "'"))

                (new-rational :set-numerator! (long-integer num-part))
                (new-rational :set-denominator! (long-integer den-part))
                (new-rational :set-express-type! 'rational)))

          ;; 小数
          (else
            (define (is-valid-char-for-decimal? c)
              (or (c :digit?) (c :equals #\.)))

            (let* ((len (string-length num-str))
                   (first-char (if (> len 0) (string-ref num-str 0) #f))
                   (has-sign? (and first-char (or (char=? first-char #\+) (char=? first-char #\-))))
                   (sign-val (if (and has-sign? (char=? first-char #\-)) '- '+))
                   (num-body-str (if has-sign? (substring num-str 1 len) num-str)))

              (when (string-null? num-body-str)
                (value-error "rational-number@apply: string is only a sign or effectively empty after sign '" num-str "'"))
              (when (not ($ num-body-str :forall is-valid-char-for-decimal?))
                (value-error "rational-number@apply: decimal string part '" num-body-str "' contains invalid characters. Input: '" num-str "'"))

              (let* ((dot-idx ($ num-body-str :index-of #\.))
                     (dot-count ($ num-body-str :count (lambda (c) (c :equals #\.)))))

                (when (> dot-count 1)
                  (value-error "rational-number@apply: multiple decimal points in '" num-body-str "'. Input: '" num-str "'"))
                (when (and (integer? dot-idx) (>= dot-idx 0) 
                           (or (= dot-idx 0) 
                               (= dot-idx (- (string-length num-body-str) 1))))
                  (value-error "rational-number@apply: decimal point at start or end of number part '" num-body-str "'. Input: '" num-str "'"))

                  (let*((digits-only-str (if (and (integer? dot-idx) (>= dot-idx 0)) 
                                             ($ num-body-str :replace-first "." "" :get)
                                             num-body-str))
                        (numerator-long-int (long-integer (if (eq? sign-val '-)
                                                              (string-append "-" digits-only-str)
                                                              digits-only-str)))
                        (denominator-long-int
                          (if (and (integer? dot-idx) (>= dot-idx 0)) 
                              (let* ((fractional-part-len (- (string-length num-body-str) (+ dot-idx 1))))
                                (long-integer (string-append "1" (make-string fractional-part-len #\0))))
                              (long-integer "1")))) 
                    (new-rational :set-numerator! numerator-long-int)
                    (new-rational :set-denominator! denominator-long-int)
                    (new-rational :set-express-type! 'decimal)))))))
      (new-rational :normalize!)
      new-rational))

  (define (%to-decimal-string)
    (if (numerator :zero?)
        "0"
        (let* ((num-abs (numerator :abs))
               (den-abs (denominator :abs))
               (result-sign (if (eq? (numerator :get-sign) (denominator :get-sign)) "" "-")))
          
          (if (zero? decimal-precision)
              (string-append result-sign ((num-abs :/ den-abs) :to-string))
              
              ;; 将分子乘以10^decimal-precision，然后做整数除法
              (let* ((scale-factor (long-integer (string-append "1" (make-string decimal-precision #\0))))
                     (scaled-numerator (num-abs :* scale-factor))
                     (result-integer (scaled-numerator :/ den-abs))
                     (result-str (result-integer :to-string)))
                
                ;; 在适当位置插入小数点
                (let* ((str-len (string-length result-str))
                       (int-part-len (- str-len decimal-precision)))
                  
                  (cond
                    ;; 整数部分长度 > 0，正常插入小数点
                    ((> int-part-len 0)
                     (let* ((int-part (substring result-str 0 int-part-len))
                            (dec-part (substring result-str int-part-len str-len))
                            (trimmed-dec-part (let trim-zeros ((s dec-part))
                                                (if (and (> (string-length s) 0)
                                                         (char=? (string-ref s (- (string-length s) 1)) #\0))
                                                    (trim-zeros (substring s 0 (- (string-length s) 1)))
                                                    s))))
                       (if (string-null? trimmed-dec-part)
                           (string-append result-sign int-part)
                           (string-append result-sign int-part "." trimmed-dec-part))))
                    
                    ;; 整数部分长度 = 0，需要在前面补"0."
                    ((= int-part-len 0)
                     (let* ((trimmed-result (let trim-zeros ((s result-str))
                                              (if (and (> (string-length s) 0)
                                                       (char=? (string-ref s (- (string-length s) 1)) #\0))
                                                  (trim-zeros (substring s 0 (- (string-length s) 1)))
                                                  s))))
                       (if (string-null? trimmed-result)
                           "0"
                           (string-append result-sign "0." trimmed-result))))
                    
                    ;; 整数部分长度 < 0，需要在小数点后补前导0
                    (else
                     (let* ((leading-zeros (make-string (abs int-part-len) #\0))
                            (trimmed-result (let trim-zeros ((s result-str))
                                              (if (and (> (string-length s) 0)
                                                       (char=? (string-ref s (- (string-length s) 1)) #\0))
                                                  (trim-zeros (substring s 0 (- (string-length s) 1)))
                                                  s))))
                       (if (string-null? trimmed-result)
                           "0"
                           (string-append result-sign "0." leading-zeros trimmed-result)))))))))))

) ; end of rational-number

(define-case-class matrix ()
  (define row-count 0)
  (define col-count 0)
  ;; 矩阵的元素是rational-number
  (define data #())
  
  ;; getter方法
  (define (%get-rows-number) row-count)
  (define (%get-cols-number) col-count)
  
  ;; setter方法
  (typed-define (%set-rows! (rows integer?))
    (when (<= rows 0)
      (value-error "matrix%set-rows!: rows must be positive"))
    (set! row-count rows))
  
  (typed-define (%set-cols! (cols integer?))
    (when (<= cols 0)
      (value-error "matrix%set-cols!: cols must be positive"))
    (set! col-count cols))
  
  (typed-define (%set-data! (new-data vector?))
    (set! data new-data))
  
  ;; 内部索引转换：将二维索引转换为一维索引
  (define (index->linear row col)
    (+ (* row col-count) col))
  
  ;; 返回矩阵的第row行，第col列的元素(rational-number)
  (typed-define (%get (row integer?) (col integer?))
    (when (or (< row 0) (>= row row-count) (< col 0) (>= col col-count))
      (value-error "matrix%get: index out of bounds"))
    (vector-ref data (index->linear row col)))
  
  (typed-define (%set! (row integer?) (col integer?) (val any?))
    (when (or (< row 0) (>= row row-count) (< col 0) (>= col col-count))
      (value-error "matrix%set!: index out of bounds"))
    (unless (rational-number :is-type-of val)
      (value-error "matrix%set!: value must be rational-number"))
    (vector-set! data (index->linear row col) val))
  
  ;; 从嵌套列表构造矩阵，内部元素为string
  (typed-define (@form-list (lst list?))
    (when (null? lst)
      (value-error "matrix@form-list: list cannot be empty"))
    
    (let* ((rows (length lst))
           (first-row (car lst))
           (cols (if (list? first-row) (length first-row) 1)))
      
      (when (<= cols 0)
        (value-error "matrix@form-list: each row must be non-empty"))
      
      (unless (every (lambda (row) 
                       (and (list? row) (= (length row) cols)))
                     lst)
        (value-error "matrix@form-list: all rows must have the same length"))
      
      (let ((result (matrix)))
        (result :set-rows! rows)
        (result :set-cols! cols)
        (result :set-data! (make-vector (* rows cols)))
        
        (do ((i 0 (+ i 1)))
            ((= i rows))
          (let ((row-data (list-ref lst i)))
            (do ((j 0 (+ j 1)))
                ((= j cols))
              (let* ((element (list-ref row-data j))
                     (rational-val (if (string? element)
                                       (rational-number element)
                                       (value-error "matrix@form-list: elements must be strings"))))
                (result :set! i j rational-val)))))
        result)))
  
  ;; 构造零矩阵
  (typed-define (@zeros (rows integer?) (cols integer?))
    (when (or (<= rows 0) (<= cols 0))
      (value-error "matrix@zeros: rows and cols must be positive"))
    
    (let ((result (matrix)))
      (result :set-rows! rows)
      (result :set-cols! cols)
      (result :set-data! (make-vector (* rows cols) (rational-number "0")))
      result))
  
  ;; 构造单位矩阵
  (typed-define (@identity (n integer?))
    (when (<= n 0)
      (value-error "matrix@identity: n must be positive"))
    
    (let ((result (matrix :zeros n n)))
      (do ((i 0 (+ i 1)))
          ((= i n))
        (result :set! i i (rational-number "1")))
      result))
  
  ;; 转换为嵌套列表，内部元素为string
  (define (%to-list)
    (let ((result-list '()))
      (do ((i (- row-count 1) (- i 1)))
          ((< i 0))
        (let ((row-list '()))
          (do ((j (- col-count 1) (- j 1)))
              ((< j 0))
            (set! row-list (cons ((%get i j) :to-string) row-list)))
          (set! result-list (cons row-list result-list))))
      result-list))

  ;; 复制矩阵
  (define (%copy)
    (let ((result (matrix :zeros row-count col-count)))
      (do ((i 0 (+ i 1)))
          ((= i row-count))
        (do ((j 0 (+ j 1)))
            ((= j col-count))
          (result :set! i j (%get i j))))
      result))

  ;; 矩阵加法
  (define (%+ that)
    (unless (matrix :is-type-of that)
      (value-error "matrix%+: argument must be a matrix"))
    
    (let ((that-rows (that :get-rows-number))
          (that-cols (that :get-cols-number)))
      (unless (and (= row-count that-rows) (= col-count that-cols))
        (value-error "matrix%+: matrices must have the same dimensions"))
      
      (let ((result (matrix :zeros row-count col-count)))
        (do ((i 0 (+ i 1)))
            ((= i row-count))
          (do ((j 0 (+ j 1)))
              ((= j col-count))
            (let ((sum ((%get i j) :+ (that :get i j))))
              (result :set! i j sum))))
        result)))

  ;; 矩阵减法
  (define (%- that)
    (unless (matrix :is-type-of that)
      (value-error "matrix%-: argument must be a matrix"))
    
    (let ((that-rows (that :get-rows-number))
          (that-cols (that :get-cols-number)))
      (unless (and (= row-count that-rows) (= col-count that-cols))
        (value-error "matrix%-: matrices must have the same dimensions"))
      
      (let ((result (matrix :zeros row-count col-count)))
        (do ((i 0 (+ i 1)))
            ((= i row-count))
          (do ((j 0 (+ j 1)))
              ((= j col-count))
            (let ((diff ((%get i j) :- (that :get i j))))
              (result :set! i j diff))))
        result)))

  ;; 矩阵乘法
  (define (%* that)
    (unless (matrix :is-type-of that)
      (value-error "matrix%*: argument must be a matrix"))
    
    (let ((that-rows (that :get-rows-number))
          (that-cols (that :get-cols-number)))
      (unless (= col-count that-rows)
        (value-error "matrix%*: incompatible dimensions for multiplication"))
      
      (let ((result (matrix :zeros row-count that-cols)))
        (do ((i 0 (+ i 1)))
            ((= i row-count))
          (do ((j 0 (+ j 1)))
              ((= j that-cols))
            ;; result[i][j] = sum this[i][k] * that[k][j]
            (let ((sum (rational-number "0")))
              (do ((k 0 (+ k 1)))
                  ((= k col-count))
                (let ((product ((%get i k) :* (that :get k j))))
                  (set! sum (sum :+ product))))
              (result :set! i j sum))))
        result)))

  ;; 矩阵转置
  (define (%transpose)
    (let ((result (matrix :zeros col-count row-count)))
      (do ((i 0 (+ i 1)))
          ((= i row-count))
        (do ((j 0 (+ j 1)))
            ((= j col-count))
          ;; result[j][i] = this[i][j]
          (result :set! j i (%get i j))))
      result))

  ;; 矩阵的非负整数幂运算
  (define (%^ n)
    (unless (integer? n)
      (value-error "matrix%^: exponent must be an integer" n))
    
    (unless (>= n 0)
      (value-error "matrix%^: exponent must be a non-negative integer" n))
    
    (unless (= row-count col-count)
      (value-error "matrix%^: matrix must be square for exponentiation"))
    
    (cond
      ((= n 0) (matrix :identity row-count))
      ((= n 1) (%copy))
      (else
        (let ((result (matrix :identity row-count))
              (base (%copy))
              (exponent n))
          (let loop ((result-acc result) (base-acc base) (exp exponent))
            (cond
              ((= exp 0) result-acc)
              ((odd? exp)
               (loop (result-acc :* base-acc) 
                     base-acc 
                     (- exp 1)))
              (else
                (loop result-acc 
                      (base-acc :* base-acc) 
                      (quotient exp 2)))))))))

) ; end of matrix

) ; begin
) ; define-library
