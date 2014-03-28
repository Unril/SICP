#lang racket

(define (sum term a next b )
  (define (iter curr result)
    (if (> curr b)
        result
        (iter (next curr) (+ (term curr) result))))
  (iter a 0))

(define (pi-sum a b)
  (sum (λ (x) (/ 1.0 
                 (* x (+ x 2))))
       a
       (λ (x) (+ x 4))
       b))

(* 8 (pi-sum 1 100))

(define y 10)

(define (foo y) (+ 100 y))

(foo y)

(+ (let ((y 3)
         (x (+ 10 y)))
     (+ y (* x 10)))
   y)

