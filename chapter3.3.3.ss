#lang scheme
(require r5rs)
(define (lookup key table)
  (let ((record (assoc-t key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc-t key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc-t key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc-t key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))
(define (make-table)
  (list '*table*))

(define (lookup-2 key-1 key-2 table)
  (let ((subtable (assoc-t key-1 (cdr table))))
    (if subtable
        (let ((record (assoc-t key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert-2 key-1 key-2 value table)
  (let ((subtable (assoc-t key-1 (cdr table))))
    (if subtable
        (let ((record (assoc-t key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
        (set-cdr! table 
                  (cons (cons key-1 (cons (cons key-2 value) '())) (cdr table)))))
  'ok)


(define q2 (make-table))
(insert-2 'letter 'math 'hello q2)
(lookup-2 'letter 'math q2)
(lookup-2 'letter 'math2 q2)

(define (make-table-o)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc-t key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-t key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc-t key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-t key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table 
                      (cons (cons key-1 (cons (cons key-2 value) '())) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation --Table" m))))
    dispatch))

;; ex 3.24
(define (same-key? key1 key2)
  (if (< (abs (- (abs key1) (abs key2))) 0.001)
      #t
      #f))
;; ex 3.26
(define (create-record key value left-branch right-branch)
  (cons (cons key value) (cons left-branch right-branch)))

(define (get-key record)
  (caar record))
(define (get-value record)
  (cdar record))
(define (get-left-branch record)
  (cadr record))
(define (get-right-branch record)
  (cddr record))

(define (set-key! record key)
  (set-car! (car record) key))
(define (set-value! record value)
  (set-cdr! (car record) value))
(define (set-left-branch! record left)
  (set-car! (cdr record) left))
(define (set-right-branch! record right)
  (set-cdr! (cdr record) right))

(define (insert-record! record key value)
  (set-key! record key)
  (set-value! record value))

(define (empty? record)
  (and (equal? '() (get-key record)) (equal? '() (get-value record))
       (equal? '() (get-left-branch record)) (equal? '() (get-right-branch record))))

(define (lookup-t table key)
  (if (equal? table '())
      (display "Not exit this key")      
      (let ((this-key (get-key table)))
         (cond ((= key this-key)
                (get-value table))
               ((< key this-key)
                (lookup-t (get-left-branch table) key))
               ((> key this-key)
                (lookup-t (get-right-branch table) key))
               (else "No exit")))))

(define (insert-t! table key value)
  (if (empty? table)
      (insert-record! table key value)
      (let ((this-key (get-key table)))
        (cond ((= key this-key)
               (set-value! table value))
              ((< key this-key)
               (insert-branch! table key value get-left-branch set-left-branch!))
              ((> key this-key)
               (insert-branch! table key value get-right-branch set-right-branch!))
              (error "")))))
(define (insert-branch! table key value get-branch set-branch!)
  (cond ((equal? '() (get-branch table))
         (let ((new-record (create-record key value '() '())))
           (set-branch! table new-record)))
        (else (insert-t! (get-branch table) key value))))

(define (make-table-t)
  (create-record '() '() '() '()))

(define t1 (make-table-t))
(insert-t! t1 1 'a)
(insert-t! t1 2 'b)
(lookup-t t1 2)
(lookup-t t1 3)








