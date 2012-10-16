#lang racket
(define nil '())
(define (map proc items)
  (if (not (pair? items))
      items
      (cons (proc (car items)) (map proc (cdr items)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
            (accumulate op initial (cdr sequence)))))
         
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high)))) 

;;  problem : 八皇后问题
;;  answer: 

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (if (null? rest-of-queens)
      (list (cons k new-row))
      (cons (cons k new-row) rest-of-queens)))

(adjoin-position 1 2 (list 3 4 5))

(define (remove-target-column column board)
  (filter (lambda (x) (not (= (car x) column)))
          board))
(define (get-target-column column board)
  (car (filter (lambda (x) (= (car x) column)))
       (board)))

(define (is-check? pos1 pos2)
  (cond ((= (car pos1) (car pos2)) #t)
        ((= (cdr pos1) (cdr pos2)) #t)
        ((= (abs (- (car pos1) (car pos2))) (abs (- (cdr pos1) (cdr pos2)))) #t)
        (else #f)))
(define (board-checks? pos board)
  (cond ((null? board) #t)
        ((is-check? pos (car board)) #f)
        (else (board-checks? pos (cdr board)))))
(define (safe? x y)
  (board-checks? (get-target-column x y) (remove-target-column x y)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))