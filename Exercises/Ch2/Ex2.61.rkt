#lang racket

(require "p2.3.3lib.rkt")

(adjoin-oset 1 null)
(adjoin-oset 1 '(1))
(adjoin-oset 1 '(2))
(adjoin-oset 2 '(1))
(adjoin-oset 3 '(1 2 4 5))
(adjoin-oset 30 '(1 2 4 5))
(adjoin-oset 0 '(1 2 4 5))