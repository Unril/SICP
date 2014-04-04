#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))
(define (print-interval z)
  (printf "(~a, ~a)~n" (lower-bound z) (upper-bound z)))

(define (width z) 
  (/ (- (upper-bound z) (lower-bound z)) 2.0))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

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




(define i1 (make-interval 1 2))
(define i2 (make-interval 3 5))

(define i3 (make-interval 4 5))
(define i4 (make-interval -3 -1))

(width i1)
(width i2)
(newline)
(width i3)
(width i4)
(newline)

(width (add-interval i1 i2))
(width (add-interval i3 i4))
(newline)

(width (sub-interval i1 i2))
(width (sub-interval i3 i4))
(newline)

(width (mul-interval i1 i2))
(width (mul-interval i3 i4))
(newline)

(width (div-interval i1 i2))
(width (div-interval i3 i4))
(newline)
