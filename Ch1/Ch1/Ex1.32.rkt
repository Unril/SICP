#lang racket

(define (accum comb start term a next b)
  (define (iter cur res)    
    (if (> cur b)
        res
        (iter (next cur) (comb (term cur) res))))
  (iter a start))

(define (sum-plus a b)  
  (define (next x) (+ x 1))
  (define (term x) x)
  (accum + 0 term a next b))

(sum-plus 0 10)
(sum-plus 0 100)