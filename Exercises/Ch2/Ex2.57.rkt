#lang racket

(require "p2.3.2lib.rkt")

; (+ (* x y) (* y (+ x 3)))
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)