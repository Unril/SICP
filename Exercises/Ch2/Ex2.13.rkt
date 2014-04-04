#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

(define (format-interval z)
  (format "(~a, ~a)~n" (lower-bound z) (upper-bound z)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-center-width c (* p c (/ 1 100))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* 100 (/ (width i) (center i))))

(define (eq-interval x y)
  (and (= (lower-bound x) (lower-bound y)) (= (upper-bound x) (upper-bound y))))

(define (cross-zero? z) 
  (<= (* (upper-bound z) (lower-bound z)) 0))

; Functions

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (f-interval f x y)
  (let ((p1 (f (lower-bound x) (lower-bound y)))
        (p2 (f (lower-bound x) (upper-bound y)))
        (p3 (f (upper-bound x) (lower-bound y)))
        (p4 (f (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (f-interval * x y))

(define (div-interval x y)
  (if (cross-zero? y)
      (error 'div-interval "Divider corsses zero! ~a" (format-interval y))
      (f-interval / x y)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (par3 r1 r2)
  (f-interval (λ (x1 x2) 
                (/ (* x1 x2 ) (+ x1 x2))) r1 r2))


(define (par4 r1 r2)
  (f-interval (λ (x1 x2) 
                (/ 1 (+ (/ 1 x1) (/ 1 x2)))) r1 r2))

; Tests

(define i1 (make-interval 2 4))
(define i2 (make-interval 1 6))

(par1 i1 i1)
(par2 i1 i1)
(par3 i1 i1)
(par4 i1 i1)

(newline)

(par1 i1 i2)
(par2 i1 i2)
(par3 i1 i2)
(par4 i1 i2)