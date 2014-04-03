#lang racket

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (f-rec (- n 2))
         (f-rec (- n 3)))))

(f-rec 1)
(f-rec 3)
(f-rec 10)
(f-rec 15)

(define (f-iter n)
  (define (iter cur prev prev-prev count)
    (if (<= count 0)
        prev-prev
        (iter (+ cur prev prev-prev) cur prev (- count 1))))  
  (iter 2 1 0 n))

(newline)
(f-iter 0)
(f-iter 1)
(f-iter 2)
(f-iter 3)
(f-iter 10)
(f-iter 15)