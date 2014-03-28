#lang racket

(define (close? a b)
  (< (abs (- a b)) 0.0001))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close? guess next)
          next
          (try next))))
  (try first-guess))

(define (gold)
  (fixed-point (λ (x) (+ 1.0 (/ 1.0 x))) 1.0))

(gold)