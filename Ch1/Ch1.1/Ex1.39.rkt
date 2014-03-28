#lang racket

(define (cont-frac get-n get-d k)
  (define (iter curr i)
    (if (= 0 i)
        curr
        (let ((n (get-n i))
              (d (get-d i)))
          (iter (/ n (+ d curr)) (- i 1)))))
  (iter 0.0 k))

(define (tan-cf x k)
  (- (/ (cont-frac (λ (i) (* -1 x x))
                   (λ (i) (- (* i 2) 1))
                   k) 
        x)))

(tan-cf (/ pi 3) 11)
(tan (/ pi 3))