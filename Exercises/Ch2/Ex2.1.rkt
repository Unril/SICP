#lang racket

(define (add-r x y)
  (r (+ (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))

(define (mul-r x y)
  (r (* (numer x) (numer y))
     (* (denom x) (denom y))))

(define (eq-r? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (r n d)
  (cons (* (sgn (* n d)) (abs n)) (abs d)))


(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-r x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-r (add-r (r 1 2) (r 1 3)))
(print-r (mul-r (r 1 2) (r 1 3)))

(print-r (r -1 -2))
(print-r (r 1 -2))
(print-r (r -1 2))