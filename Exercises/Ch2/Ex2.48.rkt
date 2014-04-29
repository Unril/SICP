#lang racket

(require "p2.2.4lib.rkt"
         "../my-lib.rkt")

(define s (make-segment (make-vect 1 2) (make-vect 3 4)))
s
(segment-start s)
(segment-end s)