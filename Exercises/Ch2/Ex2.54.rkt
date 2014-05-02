#lang racket

{define (equal-l? ls1 ls2)
  (cond [(and (null? ls1) (null? ls2)) #t]
        [(or (null? ls1) (null? ls2)) #f]
        [else (and (eq? (car ls1) (car ls2)) (equal-l? (cdr ls1) (cdr ls2)))])}

(equal-l? '() '())
(equal-l? '(this is a list) '(this is a list))
(equal-l? '(this is a list) '(this (is a) list))