#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

(define (format-interval z)
  (format "(~a, ~a)~n" (lower-bound z) (upper-bound z)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-center-width c (* p c 0.01)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

(define (eq-interval x y)
  (and (= (lower-bound x) (lower-bound y)) (= (upper-bound x) (upper-bound y))))

(define (mul-bounds z)
  (* (upper-bound z) (lower-bound z)))

(define (cross-zero? z) 
  (>= mul-bounds 0))

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

(percent (make-interval 1 3))
(make-center-percent 2 50)
