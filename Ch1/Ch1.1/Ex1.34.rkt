#lang racket

(define (f g)
  (g 2))

(f (λ (a) (+ 1 a)))