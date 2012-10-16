#lang racket

(define Y (lambda (f)
            (let ((g (lambda (h)
                       (lambda (x) 
                         (((f (h h)) x))))))
              (g g))))

(let ((f (Y (lambda (h)
              (lambda (n)
                (if (< n 2)
                    1
                    (* n (h (- n 1)))))))))
  (f 10))

(letrec ((f (lambda (n m)
              (if (< n 2) 
                  m
                  (f (- n 1) (* n m))))))
  (f 10 1))

(letrec ((f (lambda (n)
              (lambda (m)
                (if (< n 2)
                    m
                    ((f (- n 1)) (* m n)))))))
  ((f 10) 1))

(let ((g (lambda (h)
           (lambda (n)
             (if (< n 2)
                 1
                 (* n ((h h) (- n 1))))))))
  ((g g) 10))