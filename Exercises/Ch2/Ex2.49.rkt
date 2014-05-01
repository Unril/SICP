#lang racket

(require "p2.2.4lib.rkt"
         "../my-lib.rkt" 
         2htdp/image)

(define w 25)
(define h 25)
(define scene (empty-scene w h))
(define scene-frame (make-frame (make-vect 0 0)  (make-vect w 0)  (make-vect 0 h)))

(outline-painter scene scene-frame)
(x-painter scene scene-frame)
(rhombus-painter scene scene-frame)