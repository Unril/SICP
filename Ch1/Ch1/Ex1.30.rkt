#lang racket

(define (sum term a next b)
  (define (iter cur res)    
    (if (> cur b)
        res
        (iter (next cur) (+ (term cur) res))))
  (iter a 0))

(define (sum-plus a b)
  (define (next x) (+ x 1))
  (define (term x) x)
  (sum term a next b))

(sum-plus 0 10)
(sum-plus 0 100)