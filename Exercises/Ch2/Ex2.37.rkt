#lang racket

(define test-mat (list (list 1 2 3)
                       (list 4 5 6)
                       (list 7 8 9)))

(define test-vec-1 (list 1 2 3))
(define test-vec-2 (list 1 10 100))

{define (dot-product v1 v2)
  (apply + (map * v1 v2))}

; 321
(dot-product test-vec-1 test-vec-2)

{define (mat-*-vec mat vec)
  (map {λ (row)
         (apply + 
                (map * 
                     row
                     vec))}
       mat)}

; (321, 654, 987)
(mat-*-vec test-mat test-vec-2) 

{define (transpose mat)
  (apply map list mat)}

; {{1, 4, 7}, {2, 5, 8}, {3, 6, 9}}
(transpose test-mat) 

{define (mat-*-mat mat-1 mat-2)
  (map (curry mat-*-vec
              (transpose mat-2))
       mat-1)}

; {{30, 36, 42}, {66, 81, 96}, {102, 126, 150}}
(mat-*-mat test-mat test-mat)