#lang racket

(require "p2.2.4lib.rkt")

(define f (make-frame 1 2 3))
f
(frame-origin f)
(frame-edge1 f)
(frame-edge2 f)