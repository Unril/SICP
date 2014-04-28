#lang racket

(require "../my-lib.rkt")

(define v1 (vect 1 2))
(define v2 (vect 10 20))

(vect-sub (vect-scale (vect-add v1 v2) 2) v1)