#lang scheme
;; exercise3
(require test-engine/scheme-tests)
(define sum-sqr-12
  (lambda (x y z)
    (cond
      [(and (>= x z) (>= y z)) (+ (expt x 2) (expt y 2))] ;; smallest z
      [(and (>= x y) (>= z y)) (sum-sqr-12 x z y)] ;; smallest y
      [(and (>= z x) (>= z y)) (sum-sqr-12 z y x)]))) ;; smallest x

(sum-sqr-12 1 2 3)
