#lang racket

; (2^a)(3^b)

(define (cons-p x y)
  (* (expt 2 x) (expt 3 y)))


(define (car-p z)
  (define (calc x)
    (if (= (remainder z (expt 2 x)) 0)
        (calc (+ x 1))
        (- x 1)))
  (calc 1))

(define (cdr-p z)
  (define (calc x)
    (if (= (remainder z (expt 3 x)) 0)
        (calc (+ x 1))
        (- x 1))) 
  (calc 1))

(define (print-pair z)
  (newline)
  (display (car-p z))
  (display ", ")
  (display (cdr-p z)))

(print-pair (cons-p 2 3))
(print-pair (cons-p 1 1))
(print-pair (cons-p 123 321))