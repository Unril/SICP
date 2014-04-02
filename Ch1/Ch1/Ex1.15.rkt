#lang racket

(define (cube x) (* x x x))

(define (div-sin pair) 
  (cons (- (* 3 (car pair)) 
           (* 4 (cube (car pair))))
        (cdr pair)))

(define (sine a)
  (define (impl angle count)
    (if (not (> (abs angle) 0.1))
        (cons angle count)        
        (div-sin (impl (/ angle 3.0)
                 (+ count 1)))))
  (impl a 0))

(sine (- pi))
(sine pi)
(sine (/ pi 2))
(sine 12.15)