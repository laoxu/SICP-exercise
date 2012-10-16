#lang racket
(require r5rs)


(define (append-i x y)
  (if (null? x)
      y
      (cons (car x) (append-i (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
(define v (list 'a 'b 'c 'd))
(define w (mystery v))

(define x (list 'a 'b))
(define z1 (cons x x))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define make-queue (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue")
      (car (front-ptr queue))))
(define (insert-queue queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with empty queue" queue))
         (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

;; ex 3.21
(define (print-queue queue)
  (cond ((empty-queue? queue) (display "") (newline))
        (else
         (display-queue (front-ptr queue)))))
(define (display-queue item)
  (display (car item)) (display " ")
  (if (not (eq? (cdr item) '()))
      (display-queue (cdr item))
      (newline)))

;; ex 3.23
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (forward-ptr item) (car (cdr item)))
(define (reverse-ptr item) (cdr (cdr item)))
(define (set-forward-ptr! item value)
  (set-car! (cdr item) value))
(define (set-reverse-ptr! item value)
  (set-cdr! (cdr item) value))

(define (empty-deque? deque) (null? (front-ptr deque)))
(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "Front called with an empty deque" deque)
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "Rear called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (rear-insert-deque! deque item)
  (let ((new-deque-item (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-deque-item)
           (set-rear-ptr! deque new-deque-item)
           deque)
          (else
           (set-forward-ptr! (rear-ptr deque) new-deque-item)
           (set-reverse-ptr! new-deque-item (rear-ptr deque))
           (set-rear-ptr! deque new-deque-item)
           deque))))
(define (front-insert-deque! deque item)
  (let (new-deque-item (cons item (cons '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-deque-item)
           (set-rear-ptr! deque new-deque-item)
           deque)
          (else
           (set-forward-ptr! (rear-ptr deque) new-deque-item)
           (set-reverse-ptr! new-deque-item (rear-ptr deque))
           (set-rear-ptr! deque new-deque-item)
           deque))))

(define (empty-deque! deque)
  (set-front-ptr! deque '())
  (set-rear-ptr! deque '()))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "Rear delete! called with an empty deque deque"))
        ((eq? (forward-ptr (front-ptr deque) '())
              (empty-deque! deque))
         (else
          (set-rear-ptr! deque (reverse-ptr (rear-ptr deque)))
          (set-reverse-ptr! (forward-ptr (rear-ptr deque)) '())
          (set-forward-ptr! (rear-ptr deque) '())))))
(define (front-delete-queue! deque)
  (cond ((empty-deque! deque)
         (error "front delete! called with an empty deque deque"))
        ((eq? (forward-ptr (front-ptr deque)) '())
         (empty-deque! deque))
        (else
         (set-front-ptr! deque (forward-ptr (front-ptr deque)))
         (set-forward-ptr! (reverse-ptr (front-ptr deque)) '())
         (set-reverse-ptr! (front-ptr deque) '()))))
  



         

                





