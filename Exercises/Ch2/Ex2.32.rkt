#lang racket

{define (subsets items)
  (if (null? items)
      (list null)
      (let ([rest (subsets (cdr items))]
            [current (car items)])
        (append rest
                (map {λ (subset)                       
                       (cons current subset)}
                     rest))))}

(subsets null)
(subsets (list 1))
(subsets (list 1 2))
(subsets (list 1 2 3))