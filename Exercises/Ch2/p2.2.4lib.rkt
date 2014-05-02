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

{define (draw-line scene vect1 vect2)
  (add-line scene 
            (vect-x vect1) (vect-y vect1)
            (vect-x vect2) (vect-y vect2)
            "black")}


{define (seqments->painter segments)
  {λ (scene frame)
    (foldl
     {λ (segment scene)
       (draw-line scene
                  ((frame-coord-map frame) (segment-start segment))
                  ((frame-coord-map frame) (segment-end segment)))}
     scene
     segments)}}

(provide outline-painter
         x-painter
         rhombus-painter
         z-painter)

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

(define z-painter
  (seqments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5) (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5) (make-vect 1 1)))))

(provide transform-painter
         flip-vert
         flip-horiz
         shrink-to-upper-right
         rotate90
         rotate180
         rotate270
         place-beside
         place-below)

{define (transform-painter painter origin corner1 corner2)
  {λ (scene frame)
    (let ([coord-map (frame-coord-map frame)])
      (let ([new-origin (coord-map origin)])
        (painter
         scene
         (make-frame new-origin
                     (vect-sub (coord-map corner1) new-origin)
                     (vect-sub (coord-map corner2) new-origin)))))}}

{define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0))}

{define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1))}

{define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1 0.5)
                     (make-vect 0.5 1))}

{define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0))}

{define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0))}

{define (rotate270 painter)
  (rotate180 (rotate90 painter))}

{define (place-beside painter1 painter2)
  (let ([split-point (make-vect 0.5 0)])
    (let ([paint-left (transform-painter painter1
                                         (make-vect 0 0)
                                         split-point
                                         (make-vect 0 1))]
          [paint-right (transform-painter painter2
                                          split-point
                                          (make-vect 1 0)
                                          (make-vect 0.5 1))])
      {λ (scene frame)
        (paint-left (paint-right scene frame) frame)}))}

{define (place-below painter1 painter2)
  (let ([split-point (make-vect 0 0.5)])
    (let ([paint-bottom (transform-painter painter1                                         
                                           split-point
                                           (make-vect 1 0.5)
                                           (make-vect 0 1))]
          [paint-top (transform-painter painter2
                                        (make-vect 0 0)
                                        (make-vect 1 0)
                                        split-point)])
      {λ (scene frame)
        (paint-bottom (paint-top scene frame) frame)}))}










