#lang racket

(define (runtime)
  (current-inexact-milliseconds))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n) (report-prime n (- (runtime) start-time))
                    1 )
        (else 0)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (find-primes-after start n)
  (define (iter current i)
    (cond ((< i n) (iter (+ current 1) (+ i (timed-prime-test current))))))
  (iter start 0))

(find-primes-after 1000000000 3)
(find-primes-after 10000000000 3)
(find-primes-after 100000000000 3)
(find-primes-after 1000000000000 3)