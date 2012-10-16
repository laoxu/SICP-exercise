#lang scheme
;; exercise1.4
(require test-engine/scheme-tests)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(check-expect (a-plus-abs-b 1 -2) 3)

;; exercise1.11
;; recurision
(define (fn n)  
  (if (< n 3)
      n
      (+ (fn ((- n 1))) (* 2 (fn (- n 2))) (* 3 (fn (- n 3))))))

;; iteration
(define (fn-i n)
  (define (fn-ii f0 f1 f2 count)
    (if (= count 0)
        f0
        (fn-i f1 f2 (+ f2 (* 2 f1) (* 3 f0)) (- count 1))))
  (fn-ii 0 1 2 n))
;; exercise 1.12
(define (pas-tri l n)
  (cond
    [(and (= 0 1) (<= n 0)) 1]
    [(or (= 0 n) (= n l)) 1]
    [else (+ (pas-tri (- l 1) (- n 1)) (pas-tri (- l 1) n))]))

;; ex 1.16
(define (fast-expt b n)
  (cond 
    ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))
(define (even? n)
   (= (remainder n 2) 0))
(define (square b)
  (* b b))
(check-expect (fast-expt 2 2) 4)

(define (fast-expt-i b n)
  (define (fast-i b n c)
    (cond
      [(= n 0) c]
      [(= n 1) (* c b)]
      [(even? n) (fast-i (square b) (/ n 2) c)]
      [(odd? n) (fast-i (square b) (/ (- n 1) 2) (* b c))]))
  (fast-i b n 1))

(define (odd? n)
  (not (= (remainder n 2) 0)))

;; ex 1.17
(define (fast* a b)
  (cond
    [(= b 1) a]
    [(even? b) (double (fast* a (halve b)))]
    [else (+ (fast* a (- b 1)) a)]))
(define (double b)
  (* b 2))
(define (halve b)
  (/ b 2))
;; ex 1.8
(define (fast*-i a b)
  (fast*-iter 0 a b))

(define (fast*-iter r a b)
  (cond
    [(= b 0) r]
    [(= b 1) (+ r a)]
    [(even? b) (fast*-iter r (double a) (halve b))]
    [(odd? b) (fast*-iter (+ r a) (double a) (halve (- b 1)))]))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

;; ex 1.22
(define (search-for-primes n cnt)
  (cond
    [(= cnt 0) (display "the end\n")]
    [(prime? n) (display n) (newline) (search-for-primes (+ n 1) (- cnt 1))]
    [else (search-for-primes (+ n 1) cnt)]))

;; 1.3
(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b)))))

(define (cube x)
  (* x x x))
(define (inc n) (+ n 1))
(define (inc2 n) (+ n 2))
(define (identity x) x)
(define (sum-cubes a b)
  (sum cube a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sum-i-cubes a b)
  (sum-i cube a inc b))

;; ex 1.31
(define product
  (lambda (term a next b)
    (if 
     (> a b)
     1
     (* (term a) (product term (next a) next b)))))

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define factorial-prod
  (lambda (n)
    (product identity 1 inc n)))

(define factorial-prod-i
  (lambda (n)
    (product-i identity 1 inc n)))

(define pi-d-4-term
  (lambda (n)
    (/ (* n (+ n 2)) (square (+ n 1)))))

(define pi-d-4
  (lambda (n)
    (product pi-d-4-term 2 inc2 n)))
(define accumulate
  (lambda (combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b)))))

(define sum-acc
  (lambda (term a next b)
    (accumulate + 0 term a next b)))
(define product-acc
  (lambda (term a next b)
    (accumulate * 1 term a next b)))

(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; ex 1.33
(define filtered-accumulate
  (lambda (filter combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term (if (filter a) a null-value))
                  (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define sum-sqr-prime-acc
  (lambda (a b)
    (filtered-accumulate prime? + 0 square a inc b)))

(define product-gcd1-acc
  (lambda (n)
    (define gcd1?
      (lambda (x) (= (gcd n x) 1)))
    (filtered-accumulate gcd1? * 1 identity 2 inc (- n 1))))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f1 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define (l-test x)
  (+ (let ((x 3))
       (+ x (* x 10)))
       x))
(define (f2 g)
  (g 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (average a b)
  (/ (+ a b) 2))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
          (search f b a))
          (else 
           (error "valuse are not of opposite sign" a b)))))

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(check-within (fixed-point cos 1.0) 0.739 0.0010)

;; ex 1.35
(check-within (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) 1.618 0.001)

;; ex 1.36
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
;; ex 1.37
(define (cont-frac n d k op)
  (define (frac n d k r)
    (if (= r k) 0
        (/ (n r)
           (op (d r)
               (frac n d k (+ r 1))))))
  (frac n d k 1))

;; ex 1.41
(define (double-f f)
  (lambda (x) (f (f x))))

(check-expect ((double-f inc) 1) 3)

;; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

(check-expect ((compose square inc) 6) 49)

;; 1.46
(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (define (iter guess)
      (let ((next (improve-guess guess)))
        (if (good-enough? guess next)
            next
            (iter next))))
  (iter guess)))
(define (sqrt x)
  ((iterative-improve (lambda (g n)
                        (< (abs (- (* n n) x)) 0.0001))
                      (lambda (g)
                        (/ (+ g (/ x g)) 2)))
   1.0))
(define (fixed-point-i f first-guess)
  (iterative-improve (lambda (g n)
                       (< (abs (- g n)) 0.00001))
                     f)
  first-guess)

(test)










        



    

