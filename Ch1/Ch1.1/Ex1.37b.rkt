#lang racket

(define (cont-frac get-n get-d k)
  (define (iter curr i)
    (if (= 0 i)
        curr
        (let ((n (get-n i))
              (d (get-d i)))
          (iter (/ n (+ d curr)) (- i 1)))))
  (iter 0.0 k))

(define k 1000)
(define g (cont-frac (λ (i) 1.0) (λ (i) 1.0) k))

(+ 1 g) 
(/ 1 g)