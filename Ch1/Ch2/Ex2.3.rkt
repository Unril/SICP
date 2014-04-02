#lang racket

(define (point x y) (cons x y))
(define (point.x this) (car this))
(define (point.y this) (cdr this))

(define (point.format this)
  (format "{'point', ~a, ~a}" 
          (point.x this) 
          (point.y this)))

(define (point.minus this other)
  (point (- (point.x this) (point.x other)) (- (point.y this) (point.y other))))

(define (rect p-top-left p-bottom-right) (cons p-top-left p-bottom-right))
(define (rect.top-left this) (car this))
(define (rect.bottom-right this) (cdr this))

(define (rect.format this)
  (format "{'rect', ~a, ~a}" 
          (point.format (rect.top-left this))
          (point.format (rect.bottom-right this))))

(define (rect.diag this)
  (point.minus (rect.top-left this) 
               (rect.bottom-right this)))

(define (rect.area this)
  (let ((diag (rect.diag this)))
    (abs (* (point.x diag) (point.y diag)))))

(define r (rect (point 1.0 2.0) (point 2.0 4.0)))

(rect.format r)
(rect.area r)

