#lang racket

(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))

(mul 1 1)
(mul 3 1)
(mul 1 3)
(mul 3 3)
(mul 30 5)

(newline)

(define (double a) (* a 2))
(define (half a) (/ a 2))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul a (half b))))
        (else (+ a (fast-mul a (- b 1))))))


(fast-mul 1 1)
(fast-mul 3 1)
(fast-mul 1 3)
(fast-mul 3 3)
(fast-mul 30 5)