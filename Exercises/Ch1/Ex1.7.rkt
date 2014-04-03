#lang racket

(define (sqrt-iter guess prev-guess x good-enough?)
  (if (good-enough? guess prev-guess x)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x
                 good-enough?)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough-const? guess prev-guess x)
  (< (abs (- (square guess) x))
     0.001))

(define (square x)
  (* x x))

(define (sqrt1 x)
  (sqrt-iter 1.0 x x good-enough-const?))

(square (sqrt1 0.0001))
(square (sqrt1 1))
(square (sqrt1 2))
(square (sqrt1 4))
(square (sqrt1 4000000000))

(newline)

(define (good-enough-delta? guess prev-guess x)
  (< (abs (- guess prev-guess))
     0.001))

(define (sqrt2 x)
  (sqrt-iter 1.0 x x good-enough-delta?))

(square (sqrt2 0.0001))
(square (sqrt2 1))
(square (sqrt2 2))
(square (sqrt2 4))
(square (sqrt2 4000000000))