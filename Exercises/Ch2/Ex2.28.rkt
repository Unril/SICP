#lang racket

(define x (list (list 10 11) 20 (list (list 30 31 32) (list 40 41 42)) (list 50 51)))

(define (fringe tree)
  (cond ((null? tree) null)
        ((list? tree) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))

(fringe x)
(fringe (list x x))