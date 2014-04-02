#lang racket

(require plot)

(define (compose f g) (λ (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

(define (smooth f dx) 
  (λ (x) 
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (foo x) (* (sin x) (sin (* x 3)) (sin (* x 4))))

(define dx 0.1)

(define (smooth-foo n) (repeat (smooth foo dx) n))

(define y (* pi 2))
(plot (list (axes)
            (function foo 0 y #:color 0 #:label "y = foo")
            (function (smooth-foo 1) 0 y #:color 1 #:label "y = smooth-1")
            (function (smooth-foo 2) 0 y #:color 2 #:label "y = smooth-2")
            (function (smooth-foo 8) 0 y #:color 3 #:label "y = smooth-3")))

