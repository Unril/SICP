#lang racket

(define (reverse items)
  (define (iter remains result)
    (if (null? remains) 
        result
        (iter (cdr remains) 
              (cons (car remains) result))))
  (iter items null))


(define x (list (list 10 11) 20 (list (list 30 31 32) (list 40 41 42)) (list 50 51)))

(define (deep-reverse items)
  (define (iter remains result)
    (cond ((null? remains) result)
          ((list? remains) (iter (cdr remains) 
                                 (cons (deep-reverse (car remains)) result)))
          (else remains)))
  (iter items null))

x
(deep-reverse x)
