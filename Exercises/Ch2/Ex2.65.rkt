#lang racket

(require "p2.3.3lib.rkt")

(define t1 (list->tree '(1 2 3 4 5 6)))
(define t2 (list->tree '(4 5 6 7 8)))

(union-tset (make-tree 1 null null) (make-tree 1 null null))
(union-tset (make-tree 1 null null) (make-tree 2 null null))
(union-tset (make-tree 2 null null) (make-tree 1 null null))

(tree->list (union-tset t1 t2)) 
(tree->list (intersection-tset t1 t2))