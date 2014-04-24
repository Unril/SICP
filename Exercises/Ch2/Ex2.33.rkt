#lang racket

{define (map p sequence)
  (foldr {λ (item accum) 
           (cons (p item) accum)} 
         null 
         sequence)}

(map add1 (list 1 2 3 4 5 6))

{define (append s1 s2)
  (foldr cons s2 s1)}

(append (list 1 2 3) (list 4 5 6))

{define (length seq)
  (foldr {λ (item accum)
           (add1 accum)}
         0
         seq)}

(length null)
(length (list 1 2 3))