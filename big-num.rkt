#lang racket

(provide big-add big-subtract big-multiply big-power-of pretty-print
         number->bignum string->bignum bignum? zero-or-one? one-block?)

(define MAX_BLOCK 1000)

;; Contract verifying the datatype of a bignum
(define (bignum? n)
  (cond [(not (list? n)) #f]
        [(not (list-of-ints? n)) #f]
        [else #t]))

;; Helper contract
(define (list-of-ints? lst)
  (cond [(empty? lst) #t]
        [(integer? (car lst)) (list-of-ints? (cdr lst))]
        [else #f]))

;; Contract to ensure a number is 0 or 1.
(define (zero-or-one? n)
  (match n
    [0 #t]
    [1 #t]
    [_ #f]))

;; Contract to ensure a number is an integer in the range of 0-999.
(define (one-block? n)
  (and (integer? n)
       (>= n 0)
       (< n 1000)))

;; Addition of two big-nums
(define/contract (big-add x y)
  (-> bignum? bignum? bignum?)
  (big-add1 x y 0))

(define/contract (big-add1 x y co)
  (-> bignum? bignum? zero-or-one? bignum?)
  (cond
    [(and (null? x) (null? y)) (if (= co 0) '() (list co))]
    [(null? x) (big-add1 (list co) y 0)]
    [(null? y) (big-add1 x (list co) 0)]
    [else
     (let* ([sum (+ (car x) (car y) co)]
            [carry (if (>= sum MAX_BLOCK) 1 0)]
            [digit (modulo sum MAX_BLOCK)]
            [rest (big-add1 (cdr x) (cdr y) carry)])  ;; Recursive call with carry
       (cons digit rest))]))  ;; Properly constructed list

;; Subtraction of two big-nums
(define/contract (big-subtract x y)
  (-> bignum? bignum? bignum?)
  (let ([lst (big-subtract1 x y 0)])
    (reverse (strip-leading-zeroes (reverse lst)))))

(define/contract (strip-leading-zeroes x)
  (-> bignum? bignum?)
  (cond
    [(null? x) '(0)]
    [(= 0 (car x)) (strip-leading-zeroes (cdr x))]
    [else x]))

;; Subtraction helper function
(define/contract (big-subtract1 x y borrow)
  (-> bignum? bignum? zero-or-one? bignum?)
  (cond
    [(null? x) (error "Negative result not allowed")]  ;; Prevents negative numbers
    [(null? y) (big-subtract1 x '(0) borrow)]  ;; If y is empty, assume zero
    [else
     (let* ([diff (- (car x) (car y) borrow)]
            [new-borrow (if (< diff 0) 1 0)]
            [adjusted-diff (if (< diff 0) (+ diff MAX_BLOCK) diff)]
            [rest (big-subtract1 (cdr x) (cdr y) new-borrow)])  ;; Recursive call
       (cons adjusted-diff rest))]))  ;; Properly constructed list

;; Returns true if two big-nums are equal
(define/contract (big-eq x y)
  (-> bignum? bignum? boolean?)
  (equal? x y))

;; Multiplication of two big-nums
(define/contract (big-multiply x y)
  (-> bignum? bignum? bignum?)
  (foldl (lambda (digit acc)
           (big-add (big-multiply1 x digit) acc))
         '(0)
         y))

;; Multiply a bignum by a single digit (0-999)
(define (big-multiply1 x digit)
  (let loop ([x x] [carry 0])
    (if (null? x)
        (if (= carry 0) '() (list carry))
        (let* ([prod (+ (* (car x) digit) carry)]
               [carry (quotient prod MAX_BLOCK)]
               [new-digit (modulo prod MAX_BLOCK)])
          (cons new-digit (loop (cdr x) carry))))))

;; Raise x to the power of y
(define/contract (big-power-of x y)
  (-> bignum? bignum? bignum?)
  (if (big-eq y '(0))
      '(1)
      (big-multiply x (big-power-of x (big-subtract y '(1))))))

;; Convert a number to a bignum
(define/contract (number->bignum n)
  (-> number? bignum?)
  (cond
    [(< n MAX_BLOCK) (list n)]
    [else
     (let ([block (modulo n MAX_BLOCK)]
           [rest (floor (/ n MAX_BLOCK))])
       (cons block (number->bignum rest)))]))

;; Convert a string to a bignum
(define/contract (string->bignum s)
  (-> string? bignum?)
  (let ([n (string->number s)])
    (number->bignum n)))

;; Display a big-num in an easy-to-read format
(define (pretty-print x)
  (let ([lst (reverse x)])
    (string-append
     (number->string (car lst))
     (pretty-print1 (cdr lst)))))

(define (pretty-print1 x)
  (cond
    [(null? x)  ""]
    [else
     (string-append (pretty-print-block (car x)) (pretty-print1 (cdr x)))]))

(define (pretty-print-block x)
  (string-append
   ","
   (cond
     [(< x 10) "00"]
     [(< x 100) "0"]
     [else ""])
   (number->string x)))

;; Example Usage:
(display (pretty-print (big-add (number->bignum 999) (number->bignum 2))))
(newline)
(display (pretty-print (big-subtract (number->bignum 1000) (number->bignum 1))))
(newline)
(display (pretty-print (big-multiply (number->bignum 999) (number->bignum 999))))
(newline)
(display (pretty-print (big-power-of (number->bignum 2) (number->bignum 10))))
(newline)
