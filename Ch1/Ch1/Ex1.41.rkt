#lang racket

(define (inc i) (+ i 1))

(define (double f) (λ (x) (f (f x))))

((double inc) 1)

(((double (double double)) inc) 5)