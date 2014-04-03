#lang racket

(require math/base)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else 
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-natural (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (runtime)
  (current-inexact-milliseconds))

(define (prime? n)
  (fast-prime? n 1000))

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