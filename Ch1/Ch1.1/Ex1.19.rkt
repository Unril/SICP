#lang racket

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count )
  (cond ((= count 0) b)
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)