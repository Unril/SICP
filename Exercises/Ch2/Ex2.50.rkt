#lang racket

(require "p2.2.4lib.rkt"
         "../my-lib.rkt" 
         2htdp/image)

(define w 50)
(define h w)
(define scene (empty-scene w h))
(define scene-frame (make-frame (make-vect 0 0)  (make-vect w 0)  (make-vect 0 h)))

(define p (shrink-to-upper-right z-painter))

(p scene scene-frame)
"flip-horiz"
((flip-horiz p) scene scene-frame)
"rotate90"
((rotate90 p) scene scene-frame)
"rotate180"
((rotate180 p) scene scene-frame)
"rotate270"
((rotate270 p) scene scene-frame)