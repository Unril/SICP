#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))
(define (format-interval z)
  (format "(~a, ~a)~n" (lower-bound z) (upper-bound z)))

(define (width z) 
  (/ (- (upper-bound z) (lower-bound z)) 2.0))

(define (cross-zero? z) 
  (<= (* (upper-bound z) (lower-bound z)) 0))

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
  (if (cross-zero? y)
      (error 'div-interval "Divider corsses zero! ~a" (format-interval y))
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define i (make-interval 1 2))
(define i2 (make-interval -1 1))
(define i3 (make-interval 0 1))
(define i4 (make-interval 1 0))

(div-interval i i2)
(div-interval i i3)
(div-interval i i4)
(div-interval i2 i)