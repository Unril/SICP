#lang racket

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

(define (print-interval z)
  (printf "(~a, ~a)~n" (lower-bound z) (upper-bound z)))


(define i1 (make-interval 1.0 2.0))
(define i2 (make-interval 3.0 4.0))
(print-interval (add-interval i1 i2))
(print-interval (mul-interval i1 i2))
(print-interval (div-interval i1 i2))