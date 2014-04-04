#lang racket

(define (last-pair-1 ls)
  (if (null? (cdr ls))
      (car ls)
      (last-pair-1 (cdr ls))))


(last-pair-1 (list 23 72 149 34))
(last-pair-1 (list 1 2 3))
(last-pair-1 (list 1))