#lang racket

(define (for-each func items)
  (define (step items) 
    (func (car items))
    (for-each func (cdr items)))
  (if (null? items)
      (void)
      (step items)))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))