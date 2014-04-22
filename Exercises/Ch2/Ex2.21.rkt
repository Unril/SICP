#lang racket

(define map
  (λ (proc items)
    (if (null? items) null
        (cons (proc (car items))
              (map proc (cdr items))))))

(define squares
  (λ items
    (map (λ (a) (* a a))
         items)))

(squares 1 2 3 4 5 10)