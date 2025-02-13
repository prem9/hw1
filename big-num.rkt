#lang racket

;; The big-num data structure is essentially a list of 3 digit numbers.

;; Exporting methods
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

;; Contract to insure a number is an integer in the range of 0-999.
(define (one-block? n)
  (and (integer? n)
       (>= n 0)
       (< n 1000)))

;; Addition of two big-nums
(define/contract (big-add x y)
  (-> bignum? bignum? bignum?)
  (big-add1 x y 0)
  )

(define/contract (big-add1 x y co)
  (-> bignum? bignum? zero-or-one? bignum?)
  (cond
    ;; If both lists are empty, the return value is either 0 or the caryover value.
    [(and (= 0 (length x)) (= 0 (length y)))
      (if (= co 0) '() (list co))]
    [(= 0 (length x))  (big-add1 (list co) y 0)]
    [(= 0 (length y))  (big-add1 x (list co) 0)]
    [else
       ;;
       ;; --- YOUR CODE HERE ---
       ;;
      (let* ([sum (+ (car x) (car y) co)]  ;; sum of current block
             [carry (if (>= sum 1000) 1 0)]  ;; Carries number to next block
             [digit (modulo sum 1000)]  ;; reduces number of list to less than 1000
             [rest (big-add1 (cdr x) (cdr y) carry)])  ;; Recursive call with carry
        (cons digit rest))]  ;; Construct the resulting list
    ))

;; Subtraction of two big-nums
(define/contract (big-subtract x y)
  (-> bignum? bignum? bignum?)
  (let ([lst (big-subtract1 x y 0)])
    (reverse (strip-leading-zeroes (reverse lst)))
  ))

(define/contract (strip-leading-zeroes x)
  (-> bignum? bignum?)
  (cond
    [(= 0 (length x)) '(0)]
    [(= 0 (car x)) (strip-leading-zeroes (cdr x))]
    [else x]
    ))

;; NOTE: there are no negative numbers with this implementation,
;; so 3 - 4 should throw an error.
(define/contract (big-subtract1 x y borrow)
  (-> bignum? bignum? zero-or-one? bignum?)
  ;;
  ;; --- YOUR CODE HERE ---
  ;;

  ;; Ensure x is not smaller than y by comparing from the most significant digit
  (when (let loop ([x (reverse x)] [y (reverse y)])
          (cond
            [(null? x) #f]  ;; If x and y are the same, return #f (not smaller)
            [(null? y) #f]  ;; If y runs out of digits first, x is greater
            [(< (car x) (car y)) #t]  ;; If any leftmost digit of x is smaller, return #t
            [(> (car x) (car y)) #f]  ;; If x is greater at any point, return #f
            [else (loop (cdr x) (cdr y))]))  ;; Continue checking the next most significant block
    (error "Negative result not allowed"))

  ;; Perform subtraction assuming x >= y
  (if (null? x) 
      '()  ;; Base case: Stop when all digits are processed
      (let* ([x-head (car x)]   ;; Get the current digit of x
             [y-head (if (null? y) 0 (car y))]   ;; Ensure y has a valid digit
             [diff (- x-head y-head borrow)]     ;; Compute the difference
             [new-borrow (if (< diff 0) 1 0)]    ;; If negative, borrow 1
             [adjusted-diff (if (< diff 0) (+ diff 1000) diff)])  ;; Adjust borrowed values
        (cons adjusted-diff (big-subtract1 (cdr x) (if (null? y) '() (cdr y)) new-borrow)))))  ;; Recursive call



;; Returns true if two big-nums are equal
(define/contract (big-eq x y)
  (-> bignum? bignum? boolean?)
  ;;
  ;; --- YOUR CODE HERE ---
  ;;
  (error "Not implemented"))

;; Decrements a bignum
(define/contract (big-dec x)
  (-> bignum? bignum?)
  (big-subtract x '(1))
  )

;; Multiplies two big-nums
(define/contract (big-multiply x y)
  (-> bignum? bignum? bignum?)
  ;;
  ;; --- YOUR CODE HERE ---
  ;;
  
  ;; Follow the same approach that you learned in
  ;; grade school for multiplying numbers, except
  ;; that a "block" is 0-999, instead of 0-9.
  ;; Consider creating a helper function that multiplies
  ;; a big-number with a integer in the range of 0-999.
  ;; Once you have that working, you can use it in your
  ;; solution here.
  
  ;; Base case: If either number is zero, return zero
  (if (or (equal? x '(0)) (equal? y '(0)))
      '(0)
      (let loop ([y y] [shift 0] [result '(0)])
        (if (null? y)
            result
            (let multiply-x ([x x] [carry 0] [partial '()])
              (if (null? x)
                  (if (= carry 0) 
                      (loop (cdr y) (+ shift 1) (big-add result (append (make-list shift 0) partial)))
                      (loop (cdr y) (+ shift 1) (big-add result (append (make-list shift 0) (cons carry partial)))))
                  (let* ([prod (+ (* (car x) (car y)) carry)]
                         [new-carry (quotient prod 1000)]
                         [new-digit (remainder prod 1000)])
                    (multiply-x (cdr x) new-carry (cons new-digit partial)))))))))
;; Raise x to the power of y
(define/contract (big-power-of x y)
  (-> bignum? bignum? bignum?)
  ;;
  ;; --- YOUR CODE HERE ---
  ;;
  
  ;; Solve this function in terms of big-multiply.  

  (cond
    ;; Base case: Anything raised to power 0 is 1
    [(equal? y '(0)) '(1)]
    
    ;; Base case: Anything raised to power 1 is itself
    [(equal? y '(1)) x]

    ;; If y is even, use exponentiation by squaring
    [(= (remainder (car y) 2) 0)
     (let loop ([n y] [carry 0] [half-y '()])
       (if (null? n)
           (let ([half-power (big-power-of x half-y)])  ;; Compute x^(y/2)
             (big-multiply half-power half-power))  ;; (x^(y/2)) * (x^(y/2))
           (let* ([val (+ (* carry 1000) (car n))]  ;; Add carry from previous step
                  [digit (quotient val 2)]
                  [new-carry (remainder val 2)])
             (loop (cdr n) new-carry (cons digit half-y)))))]

    ;; If y is odd, multiply once more
    [else
     (big-multiply x (big-power-of x (big-subtract y '(1))))]))  ;; x * (x^(y-1))

;; Dispaly a big-num in an easy to read format
(define (pretty-print x)
  (let ([lst (reverse x)])
    (string-append
     (number->string (car lst))
     (pretty-print1 (cdr lst))
     )))

(define (pretty-print1 x)
  (cond
    [(= 0 (length x))  ""]
    [else
     (string-append (pretty-print-block (car x)) (pretty-print1 (cdr x)))]
    ))

(define (pretty-print-block x)
  (string-append
   ","
   (cond
     [(< x 10) "00"]
     [(< x 100) "0"]
     [else ""])
   (number->string x)))

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
