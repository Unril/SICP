#lang racket

(define (compose f g) (λ (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

(define zero
  (λ (f)
    (λ (x) x)))

(define (add-1 n)
  (λ (f)
    (λ (x)
      (f ((n f) x)))))

(define one-1 (add-1 zero))
(define two-1 (add-1 one-1))
(define four-1 (add-1 (add-1 two-1)))

(define one-2 
  (λ (f)
    (λ (x) 
      (f x))))

(define two-2 
  (λ (f)
    (λ (x) 
      (f (f x)))))


(define add
  (λ (n m)
    (λ (f)
      (λ (x)
        ((m f) ((n f) x))))))


(define four-2 (add two-2 two-2))
(define five-2 (add four-2 one-2))

(define test (λ (x) (+ x 1)))

((zero test) 0)
((one-1 test) 0)
((one-2 test) 0)
(newline)
((two-1 test) 0)
((two-2 test) 0)
(newline)
((four-1 test) 0)
((four-2 test) 0)
(newline)
((five-2 test) 0)
(((add (add five-2 one-1) four-1) test) 0)