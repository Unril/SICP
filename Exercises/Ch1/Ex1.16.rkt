#lang racket

(define (square x) (* x x))

(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))

(expt 1 2)
(expt 2 1)
(expt 2 8)
(expt 3 4)
(expt 4 4)

(newline)

(define (expt-iter b n)
  (define (iter i acc) 
    (cond ((= i 0) acc)
          ((even? i) (iter (/ i 2) (* acc acc b)))
          (else (iter (- i 1) (* acc b)))))
  (iter n 1))


(expt-iter 1 2)
(expt-iter 2 1)
(expt-iter 2 8)
(expt-iter 3 4)
(expt-iter 4 4)