#lang racket

(define (sq-sum a b)
  (+ (* a a)
     (* b b)))

(define (max-two a b c)
  (cond ((and (> a c) (> b c)) (sq-sum a b))
        ((and (> a b) (> c b)) (sq-sum a c))
        ((and (> b a) (> c a)) (sq-sum b c))))

(max-two 3 2 1)
(max-two 1 2 3)
(max-two 1 3 2)
(max-two 2 1 3)