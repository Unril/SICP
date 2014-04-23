#lang racket

(define x (cons (list 1 2) (list 3 4)))

(length x)

(define (count-leaves tree)
  (cond
    ((null? tree) 0)
    ((pair? tree) (+ (count-leaves (car tree)) (count-leaves (cdr tree))))
    (else 1)))

(count-leaves x)
(count-leaves (cons x x))

(define y (list 1 (list 2 (list 3 4))))
y

(count-leaves y)