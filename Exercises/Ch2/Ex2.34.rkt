#lang racket

{define (eval x coeffs)
  (foldr {λ (coeff higher-terms) 
           (+ (* x higher-terms) coeff)}
         0
         coeffs)}

; 1 + 3x + 5x^3 + x^5 = 388 в точке x = 3
(eval 3 (list 1 3 0 5 0 1)) 