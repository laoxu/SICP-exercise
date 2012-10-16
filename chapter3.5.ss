#lang scheme
(require r5rs)

(define (stream-null? s)
  (null? s))
(define the-empty-stream '())
(define (force-eval delayed-object)
  (delayed-object))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (lambda () (stream-map proc (stream-cdr s))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream)
  (let ((result (cons '() '())))
    (set! result (force-eval (cdr stream)))
    result))

(define (cons-stream a b)
  (cons a (delay-memo b)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (lambda () (stream-enumerate-interval (+ low 1) high)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (lambda () stream-filter pred
                        (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (delay-memo expr)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (expr))
                 (set! already-run? #t)))
      result)))



;; ex 3.50
(define (streams-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply streams-map 
              (cons proc (map stream-cdr argstreams))))))
;; ex 3.51

(define (display-line x)
  (display x)
  (newline))
(define (show x)
  (display-line x)
  x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
;;3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
;;(display-stream z)

(define (divisible? x y)
  (= (remainder x y) 0))
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
;; ex 3.56
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams 
                                   factorials
                                   (integers-starting-form 2))))
;; ex 3.55
(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams
    (stream-cdr s)
    (partial-sums s))))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0 
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
                     
                      






