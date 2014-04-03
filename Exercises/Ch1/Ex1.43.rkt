#lang racket

(define (square x) (* x x))

(define (compose f g) (λ (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

((repeat square 2) 5)