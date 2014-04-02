#lang racket

(define (point x y) (cons x y))
(define (point.x p) (car p))
(define (point.y p) (cdr p))

(define (point.print p)
  (printf "{ ~a, ~a }~n" (point.x p) (point.y p)))

(define (avg x y) (/ (+ x y) 2))

(define (midpoint p1 p2) 
  (point (avg (point.x p1) (point.x p2)) (avg (point.y p1) (point.y p2))))

(define p1 (point 2.0 3.0))
(define p2 (point 1.0 2.0))
(point.print (midpoint p1 p2))