#lang racket

(define (map-iter proc items)
  (define (iter curr-proc remains result)
    (if (null? remains)
        result
        (iter curr-proc 
              (cdr remains)
              (cons (curr-proc (car remains)) result))))
  (iter (λ (a) a) 
        (iter proc items null)
        null))

(define squares
  (λ items
    (map-iter (λ (a) (* a a))
              items)))

(squares 1 2 3 4 5 10)