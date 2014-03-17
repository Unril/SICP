#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (next x) (+ x 1))
  (define (coeff x) 
    (cond ((= x 0) 1.0)
          ((= x n) 1.0)
          ((even? x) 2.0)
          (else 4.0)))
  (define (end x) (/ (* x h) 3.0))
  (define (term x) (* (coeff x) (y x)))
  (end (sum term 0 next n)))

(integral cube 0.0 1.0 100)
(integral cube 0.0 1.0 1000)
(integral cube 0.0 1.0 10000)
(integral cube 0.0 1.0 100000)