#lang racket

(define (prod term a next b)
  (define (iter cur res)    
    (if (> cur b)
        res
        (iter (next cur) (* (term cur) res))))
  (iter a 1))

(define (get-pi n) 
  (define (next x) (+ x 2))
  (define (term x) 
    (/ (* x (+ x 2)) 
       (* (+ 1 x) (+ 1 x))))
  (* 4.0 (prod term 2.0 next n)))

(get-pi 100)
(get-pi 1000)
(get-pi 10000)
(get-pi 100000)