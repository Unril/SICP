#lang racket

(define (square x) (* x x))

(define (inc i) (+ i 1))

(define (compose f g) (λ (x) (f (g x))))

((compose square inc) 6)