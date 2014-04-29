#lang racket

(provide unique-pairs
         unique-three
         unique-lists)

; Generate sequence of unique lists such that and 1 ≤ i < j < ... < k ≤ n
; where i j .. k are elements of the each list.
{define (unique-lists start end depth)
  {define rng (range start (add1 end))}
  (if (= 1 depth)
      (map list rng)
      (append-map {λ (i) 
                    (map {λ (rest)
                           (cons i rest)}
                         (unique-lists start (sub1 i) (sub1 depth)))}
                  rng))}

; Generate sequence of pairs (i, j) such that 1 ≤ j < i ≤ n.
{define (unique-pairs start end)
  (unique-lists start end 2)}

; Generate sequence of pairs (i, j, k) such that 1 ≤ k < j < i ≤ n.
{define (unique-three start end)
  (unique-lists start end 3)}


; ==== Vectors 2D ====

(provide make-vect
         vect-x
         vect-y
         vect-add
         vect-sub
         vect-scale)

{define (make-vect x y)
  (list x y)}

{define (vect-x v)
  (first v)}

{define (vect-y v)
  (second v)}

{define (vect-add v1 v2)
  (make-vect (+ (vect-x v1) (vect-x v2))
        (+ (vect-y v1) (vect-y v2)))}

{define (vect-neg v)
  (make-vect (- (vect-x v))
        (- (vect-y v)))}

{define (vect-sub v1 v2)
  (vect-add v1 (vect-neg v2))}

{define (vect-scale v factor)
  (make-vect (* (vect-x v) factor) 
        (* (vect-y v) factor))}
