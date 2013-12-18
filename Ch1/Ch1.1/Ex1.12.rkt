#lang racket

(define (tr row)
  (define (impl ls count)
    (if (= count row)
        ls
        (impl (map +
                   (append '(0) ls)
                   (append ls '(0)))
              (+ count 1))))
  (impl '(1) 0))

(tr 0) ; 1
(tr 1) ; 1   1
(tr 2) ; 1   2   1
(tr 3) ; 1   3   3   1
(tr 4) ; 1   4   6   4   1