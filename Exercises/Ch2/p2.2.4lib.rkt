#lang racket

(require "../my-lib.rkt" 
         2htdp/image)

(provide make-frame
         frame-origin
         frame-edge1
         frame-edge2
         frame-coord-map)


{define (make-frame origin edge1 edge2)
  (list origin edge1 edge2)}

{define (frame-origin frame)
  (first frame)}

{define (frame-edge1 frame)
  (second frame)}

{define (frame-edge2 frame)
  (third frame)}

; Origin(Frame) + x · Edge1(Frame) + y · Edge2(Frame)
{define (frame-coord-map frame)
  {λ (vect)
    (vect-add
     (frame-origin frame)
     (vect-add (vect-scale (frame-edge1 frame)
                           (vect-x vect))
               (vect-scale (frame-edge2 frame)
                           (vect-y vect))))}}

(provide make-segment
         segment-start
         segment-end)

{define (make-segment vect1 vect2)
  (list vect1 vect2)}

{define (segment-start segment)
  (first segment)}

{define (segment-end segment)
  (second segment)}

{define scale-factor 10}

{define (draw-line image vect1 vect2)
  (add-line image 
            (vect-x vect1) (vect-y vect1)
            (vect-x vect2) (vect-y vect2)
            "black")}


{define (seqments->painter image segments)
  {λ (frame)
    (foldl
     {λ (segment image)
       (draw-line image
                  ((frame-coord-map frame) (segment-start segment))
                  ((frame-coord-map frame) (segment-end segment)))}
     image
     segments)}}

(define pt (seqments->painter
            (empty-scene 100 100)
            (list (make-segment (make-vect 0 0) (make-vect 30 40)))))

;(pt (make-frame (make-vect 0 0)  (make-vect 1 0)  (make-vect 0 1)))

