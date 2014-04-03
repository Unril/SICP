#lang racket

(require math/base)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        (else 
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random-natural (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (iter n)
  (cond ((= n 1))
        (else
         (cond ((fast-prime? n  100)
                (newline)
                (display n)))
         (iter (- n 1)))))

(iter 100)