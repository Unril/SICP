#lang racket

(define (close? a b)
  (< (abs (- a b)) 0.0001))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-p-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define dx 0.00000001)

(define (deriv g)
  (λ (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (λ (x)
    (- x(/ (g x) ((deriv g) x)))))

(define (pol a b c d x)
  (+ (* a x x x) (* b x x) (* c x) d))

(define (cubic a b c d)
  (fixed-p-of-transform 
   (λ (y) (pol a b c d y)) 
   newton-transform
   1.0))

(cubic 1 1 -2 -4)