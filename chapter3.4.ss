#lang scheme
(require r5rs)

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin ((set-car! cell true)
               false))))
;; ex 3.47

(define (test cell)
  (car cell))
(define (test-and-clear! cell)
  (if (not (car cell))
      #t
      (begin (set-car! cell #f)
             #f)))
(define (make-mutex-i)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;;retry
            ((eq? m 'release) (clear! cell))
            ((eq? m 'test) (test cell))
            ((eq? m 'test-and-release) (test-and-clear! cell))))
    the-mutex))

(define (make-cells n)
  (define (make-list n max result)
    (cond ((0 n) result)
          (else 
           (begin 
             (set! result (append result (list (make-mutex-i))))
             (make-list (- n 1) max result)))))
  (make-list n n '()))
(define (make-semaphore n)
  (let ((cells (make-cells n)))
    (define (a-semaphore m)
      (cond ((eq? m 'acquire)
             (if (acquire-a-mutex! cells)
                 (a-semaphore 'acquire)))
            ((eq? m 'release) (release-a-mutex! cells))))
    a-semaphore))

(define (acquire-a-mutex! cells)
  (cond ((eq? '() cells)
         #t)
        ((not ((car cells) 'test))
         (begin ((car cells) 'acquire)
                #f))
        (else
         (acquire-a-mutex! (cdr cells)))))

(define (release-a-mutex! cells)
  (cond ((eq? '() cells)
         (error "ERROR: could not release semaphore"))
        (((car cells) 'test)
         ((car cells) 'release))
        (else
         (release-a-mutex! (cdr cells)))))
        
      
  