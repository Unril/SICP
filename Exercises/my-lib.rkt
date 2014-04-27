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