#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 2 4)
(gcd 12 15)
(gcd 100 25)

(newline)