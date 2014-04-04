#lang racket

(define (cons-l x y)
  (λ (m) (m x y)))


(define (car-l z)
  (z (λ (x y) x)))

(define (cdr-l z)
  (z (λ (x y) y)))

(define pair (cons-l 1 2))
(car-l pair)
(cdr-l pair)