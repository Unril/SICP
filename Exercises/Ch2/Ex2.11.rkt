#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))
(define (format-interval z)
  (format "(~a, ~a)~n" (lower-bound z) (upper-bound z)))

(define (eq-interval x y)
  (and (= (lower-bound x) (lower-bound y)) (= (upper-bound x) (upper-bound y))))

(define (width z) 
  (/ (- (upper-bound z) (lower-bound z)) 2.0))

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

(define (mul-interval-fast x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond [(and (>= lx 0)           (>= ly 0)) (make-interval (* lx ly) (* ux uy))]
          [(and (<  lx 0) (>= ux 0) (>= ly 0)) (make-interval (* lx uy) (* ux uy))]
          [(and (<  ux 0)           (>= ly 0)) (make-interval (* lx uy) (* ux ly))]
          [(and (<  ux 0) (<  ly 0) (>= uy 0)) (make-interval (* lx uy) (* lx ly))]
          [(and (<  ux 0)           (<  uy 0)) (make-interval (* ux uy) (* lx ly))]
          [else (mul-interval-fast y x)])))


(define (div-interval x y)
  (if (cross-zero? y)
      (error 'div-interval "Divider corsses zero! ~a" (format-interval y))
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))


(define (test x1 y1 x2 y2)
  (let ((i1 (make-interval x1 y1))
        (i2 (make-interval x2 y2)))
    (eq-interval (mul-interval i1 i2) (mul-interval-fast i1 i2))))


(test 1 2 3 4)
(test -1 2 3 4)
(test -2 -1 3 4)
(test -2 -1 -3 4)
(test -2 -1 -4 -3)
(test -2 -1 1 2)
(test -1 0 0 1)