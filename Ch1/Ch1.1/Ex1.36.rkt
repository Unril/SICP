#lang racket

(define (close? a b)
  (< (abs (- a b)) 0.0001))

(define (fixed-point f first-guess)
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close? guess next)
          (begin (newline) next)
          (try next))))
  (try first-guess))

(define (xx)
  (fixed-point (λ (x) (/ (log 1000.0) (log x))) 10.0))

(xx)