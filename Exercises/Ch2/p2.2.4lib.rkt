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
         segment-end
         seqments->painter)

{define (make-segment vect1 vect2)
  (list vect1 vect2)}

{define (segment-start segment)
  (first segment)}

{define (segment-end segment)
  (second segment)}

{define (draw-line image vect1 vect2)
  (add-line image 
            (vect-x vect1) (vect-y vect1)
            (vect-x vect2) (vect-y vect2)
            "black")}


{define (seqments->painter segments)
  {λ (image frame)
    (foldl
     {λ (segment image)
       (draw-line image
                  ((frame-coord-map frame) (segment-start segment))
                  ((frame-coord-map frame) (segment-end segment)))}
     image
     segments)}}

(provide outline-painter
         x-painter
         rhombus-painter)

(define outline-painter
  (seqments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
                           (make-segment (make-vect 1 0) (make-vect 1 1))
                           (make-segment (make-vect 1 1) (make-vect 0 1))
                           (make-segment (make-vect 0 1) (make-vect 0 0)))))

(define x-painter
  (seqments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define rhombus-painter
  (seqments->painter (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))))


