#lang racket

(require "p2.3.3lib.rkt")

(define t (make-tree 5 (make-tree 2 null
                                  (make-tree 3 null null))
                     (make-tree 8 null
                                (make-tree 9 null null))))

(tree->list t)
