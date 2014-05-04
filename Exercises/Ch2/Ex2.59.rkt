#lang racket

(require "p2.3.3lib.rkt")

(define s1 '(1 3 4 2))
(define s2 '(2 6 7 3))
(element-of-uset? 1 s1)
(element-of-uset? 0 s1)
(adjoin-uset 0 s1)
(intersection-uset s1 s2)
(union-uset s1 s2)