#lang racket

; http://dangermouse.brynmawr.edu/cs245/ycomb_jim.html

(define Y 
  (λ (func)
    ((λ (self) (func (λ (x) ((self self) x))))
     (λ (self) (func (λ (x) ((self self) x)))))))

((Y (λ (self)
      (λ (items)
        (if (null? items)
            0
            (add1 (self (cdr items)))))))
 '(1 2 3 4 50 60))