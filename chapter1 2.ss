#lang scheme
;;ex 2.1
(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))
(define (make-rat n d) (cons n d))
(define (number x) (car x))
(define (denom x) (cdr x))



;; ex 2.2
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (make-point x y)
  (cons x y))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment s e)
        (cons s e))
(define (start-seg s)
  (car s))
(define (end-seg e)
  (cdr e))

(define (midpoint-segment s e)
  (define (average a b) (/ (+ a b) 2.))
  (print-point s)
  (print-point e)
  (let ((start-point-x (x-point s))
        (start-point-y (y-point s))
        (end-point-x (x-point e))
        (end-point-y (y-point e)))
    (make-point (average start-point-x end-point-x)
                      (average start-point-y end-point-y))))
        

(midpoint-segment (make-point 2 3) (make-point 6 8))
;;ex2.3

(define (square x) (* x x))
(define (length-segment segment)
  (sqrt (+ (square (- (x-point (start-seg segment)) (x-point (end-seg segment))))
           (square (- (y-point (start-seg segment)) (y-point (end-seg segment)))))))


           
