#lang racket

(define (cont-frac get-n get-d k)
  (if (= k 0)
      0.0
      (let ((n (get-n k))
            (d (get-d k)))
        (/ n (+ d (cont-frac get-n get-d (- k 1)))))))

(define k 20)
(define g (cont-frac (λ (i) 1.0) (λ (i) 1.0) k))

(- (+ 1 g) (/ 1 g))