#lang racket
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

;;ex 2.5
(define (fast-expt base x)
  (define (expt-i result acc)
    (if (= 0 acc)
        result
        (expt-i (* base result) (- acc 1))))
  (expt-i 1 x))

(define (cons-expt x y)
  (* (fast-expt 2 x)
     (fast-expt 3 y)))
(define (factorize m base)
  (define (factorize-iter curr acc)
    (if (= 0 (remainder curr base))
        (factorize-iter (/ curr base) (+ acc 1))
        acc))
  (factorize-iter m 0))
(define (car-expt z) (factorize z 2))
(define (cdr-expt z) (factorize z 3))
;; ex 2.7
(define (make-interval a b)
  (cons (min a b) (max a b)))
(define (lower-bound interval)
  (cdr interval))
(define (upper-bound interval)
  (car interval))
;; ex 2.8
(define (sub-interval x y)
  (make-interval (abs (- (upper-bound x) (upper-bound y)))
                 (abs (- (lower-bound x) (lower-bound y)))))
;; ex 2.9
(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; ex 3.0
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (upper-bound x) (upper-bound y))))
    (make-interval (p1 p2))))

(define (div-interval x y)
  (cond ((or (zero? (lower-bound y))
             (zero? (upper-bound y))
             (and (negative? (lower-bound y)) (positive? (upper-bound y))))
         (error "divisor interval contains zero"))
        (else
         (mul-interval x
                       (make-interval (/ 1.0 (upper-bound y))
                                      (/ 1.0 (lower-bound y)))))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref '(2 3 4 5) 3)
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length-i items)
  (define (iter length a)
    (if (null? a)
        length
        (iter (+ length 1) (cdr items))))
  (iter 0 items))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;; ex 2.17
(define (last-pair ls)
  (cond ((null? ls) (error "Empty list"))
        ((null? (cdr ls)) ls)
        (last-pair (cdr ls))))
(define (last-pair1 list1)
  (if (null? (cdr list1))
      list1
      (last-pair1 (cdr list1))))

(last-pair1 (list 23 72 149 34))

;; ex 2.18
(define (reverse items)
  (define (rev items result)
    (if (null? items)
        result
        (rev (cdr items) (cons (car items) result))))
  (rev items (list)))

(reverse (list 1 4 5 9 16 22))
;; ex 2.20
(define (map proc items)
  (if (not (pair? items))
      items
      (cons (proc (car items)) (map proc (cdr items)))))


(map (lambda (x) (* x x)) (list 1 2 4))
(define (same-parity a . n)
  (define (find type n)
    (cond ((null? n) '())
          ((type (car n)) (cons (car n) (find type (cdr n))))
          (else (find type (cdr n)))))
  (if (odd? a)
      (cons a (find odd? n))
      (cons a (find even? n))))
;; ex 2.21
(define nil '())
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
(square-list (list 1 2 3 4))
(define (square-list2 items)
  (map (lambda (x) (* x x)) items))
(square-list2 (list 1 2 3 4))
;; ex 2.22
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
(square-list3 (list 1 2 3 4))
(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer 
                    (square (car things))))))
  (iter items nil))

;; ex 2.23
(define (for-each proc items)
  (cond ((null? items)
         #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

(define x (cons (list 1 2) (list 3 4)))
(length x)

(define (count-leaves x)
  (cond ((null? x) 0)
      ((not (pair? x)) 1)
      (else (+ (count-leaves (car x))
               (count-leaves (cdr x))))))
(count-leaves x)

;;ex 2.25
(cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))
(caar (list (list 7)))
;; ex 2.26
(define x1 (list 1 2 3))
(define y1 (list 4 5 6))
(append x1 y1)
(cons x1 y1)
(list x1 y1)

;; ex 2.27

(define items-r (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
  (define (d-rev items result)
    (cond ((null? items) result)
          ((not (pair? items)) items)
          (else (d-rev (cdr items) (cons (d-rev (car items) (list)) result)))))
  (d-rev items nil))
(reverse items-r)
(deep-reverse items-r)
;; ex 2.28
(define (fringe items)
  (cond ((null? items) (list))
        ((not (pair? items)) (list items))
        (else (append (fringe (car items)) (fringe (cdr items))))))

(define (fringe-i items)
  (define (accum items res)
    (cond
      ((null? items) res)
      ((not (pair? items)) (cons items res))
      (else (accum (car items) (accum (cdr items) res)))))
  (accum items null))

;; ex 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))
(define (total-weight mobile)
  (define (iter items res)
    (cond ((null? items) res)
          ((not (pair? items)) (+ res items))
          (else (iter (right-branch items) res))))
  (iter mobile nil))

(define (branch-weight branch)
  (if (not (pair? (branch-structure branch)))
      (branch-structure branch)
      (total-weight1 (branch-structure branch))))
(define (total-weight1 mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
(define (balanced? mobile)
  (define (has-sub? branch) (pair? (branch-structure branch)))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (if (has-sub? left)
             (balanced? (branch-structure left))
              #t)
         (if (has-sub? right)
             (balanced? (branch-structure right))
             #t)
         (= (* (branch-length left) (branch-weight left))
            (* (branch-length right) (branch-weight right))))))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))


;;ex 2.30
(define (square-tree1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree) (square tree)))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))
(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))))
;;ex 2.31
(define (tree-map proc tree)
  (if (pair? tree)
      (map (lambda (sub-tree) (tree-map proc sub-tree)) tree)
      (proc tree)))

(define (square-tree3 tree)
  (tree-map square tree))
(square-tree3 (list 2 3))

;; from 2.2.3
(map square (list 1 2 3 4 5))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
         
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
            (accumulate op initial (cdr sequence)))))


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high)))) 
(enumerate-interval 1 10)
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        (not (pair? tree) (list tree))
        (else (append (enumerate-tree (car tree))
                    (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
  
(define (sum-odd-square tree)
  (accumulate + 0 
              (map square 
                   (filter odd? 
                           (enumerate-tree tree))))) 

(define (even-fibs n)
  (accumulate cons nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (fib n)
  (cond ((= n 1) 1)
        ((= n 2) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; ex 2.33
(define (map-1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y) nil sequence)))
(define (append-1 seq1 seq2)
  (accumulate cons seq1 seq2))

(define (length-1 sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;; ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))
;; ex 2.35

(define (count-leaves-a t)
  (accumulate +
              0
              (map (lambda (x) 1)
                   (enumerate-tree t))))

;; ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init
                        (map car x))
            (accumulate-n op init
                          (map cons (cdr x))))))

;; ex 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;; ex 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(fold-left / 1 (list 1 2 3 4))
(accumulate / 1 (list 1 2 3 4))

;; ex 2.39

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
(define (reverse1 sequence)
  (accumulate (lambda (x y) (append y (list x)) nil sequence)))
(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 10)))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum 
        (filter prime-sum?
                (flatmap 
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n)))))

(prime-sum-pairs 10)
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (i)
                 (map (lambda (j) (cons i j))
                      (permutations (remove i s))))
               s)))

;; ex 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;; ex 2.41  all - 相同的过滤掉
;; error
(define (ordered-triple n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 (- n 1))))
                        

(define (sum seq)
  (if (null? seq)
      0
   (+ (car seq) (sum (cdr seq)))))

(filter (lambda (l)
          (= 33 (sum l)))
        (ordered-triple 23))

;; ex 2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
        (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
        (* s (ycor-vect v))))

;; ex 2.48
(define (make-segment1 start-seg end-seg)
  (cons start-seg end-seg))

(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

;; ex 

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))

;; ex 2.54
(define (equal? a b)
  (if (and (pair? x) (pair? b))
      (and (equal? (car a) (cdr b))
           (equal? (cdr a) (cdr b)))
      (eq? a b)))

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else 
         (error "unknow expression type --DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) 'x)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))






