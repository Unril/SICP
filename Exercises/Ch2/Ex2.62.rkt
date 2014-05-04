#lang racket

(require "p2.3.3lib.rkt")

(define s1 '(1 2 3 4))
(define s2 '(3 4 5 6))

(union-oset null null)
(union-oset null s2)
(union-oset s1 null)

(union-oset '(1) '(1))
(union-oset '(1) '(1 2))
(union-oset '(1 2) '(1))
(union-oset '(1 2) '(1 2))
"diff"
(union-oset '(1) '(2))
(union-oset '(2) '(1))
(union-oset s1 s2)
(union-oset s2 s1)