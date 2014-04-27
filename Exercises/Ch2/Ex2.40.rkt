#lang racket

{define (unique-pairs n)
  {define i-rng (range 1 (add1 n))}
  {define (j-rng i) (range 1 i)}
  (append-map {λ (i) 
                (map {λ (j)
                      (list i j)
                       } (j-rng i))
                } i-rng)}

(unique-pairs 5)