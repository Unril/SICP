#lang racket

(define (f x y . z) 
  (newline) 
  (display (list x y z)))


(define (g . z) 
  (newline) 
  (display z))

(define fl (λ (x y . z) 
             (newline) 
             (display (list x y z))))

(define gl (λ z 
             (newline) 
             (display z)))

(define (same-parity a . other)
  (define (pred? item) (= (remainder item 2) (remainder a 2)))
  (define (filter items) 
    (cond
      ((null? items) items)
      ((pred? (car items))
       (cons (car items) (filter (cdr items))))
      (else (filter (cdr items)))))  
  (filter (cons a other)))

(same-parity 2 2 2 2 2)
(same-parity 1)
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)