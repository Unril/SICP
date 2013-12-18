#lang racket

(define (average x y)
  (/ (+ x y) 2))

(define (cube x)
  (* x x x))

(define (sqrt3 x)
  (define (sqrt-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (sqrt-iter (improve guess) guess)))
  (define (improve guess)
    (/ (+ (/ x 
             (* guess guess))
          (* 2 guess))
       3))  
  (define (good-enough? guess prev-guess)
    (< (abs (- guess prev-guess))
       0.001))
  (sqrt-iter 1.0 x))

(sqrt3 8)
(sqrt3 1)
(newline)
(cube (sqrt3 0.0001))
(cube (sqrt3 1))
(cube (sqrt3 2))
(cube (sqrt3 4))
(cube (sqrt3 4000000000))