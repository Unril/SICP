#lang racket

(require "p2.3.2lib.rkt")

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

"exp"
(deriv '(^ x 2) 'x)
(deriv '(^ (+ x 4) 12) 'x)
(deriv '(^ (* x 10) 2) 'x)