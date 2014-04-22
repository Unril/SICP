#lang racket

(define (reverse-1 items) 
  (cond  ((null? items) null) 
         ((null? (cdr items)) (list (car items)))
         ((null? (cdr (cdr items))) (list (car (cdr items)) (car items)))
         (else (append (reverse-1 (cdr items)) (list (car items))))))

(reverse-1 (list))
(reverse-1 (list 1))
(reverse-1 (list 1 2))
(reverse-1 (list 1 4 9 16 25))