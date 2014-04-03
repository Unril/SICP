#lang racket

(define (double a) (* a 2))
(define (half a) (/ a 2))

(define (fast-mul-iter a b)
  (define (iter accum n)
    (cond ((= n 0) accum)
          ((even? n) (iter (double accum) (half n)))
          (else (iter (+ accum a) (- n 1)))))
  (iter 0 b))

(fast-mul-iter 1 1)
(fast-mul-iter 3 1)
(fast-mul-iter 1 3)
(fast-mul-iter 3 3)
(fast-mul-iter 30 5)