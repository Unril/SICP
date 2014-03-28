#lang racket

(define (average a b) (/ (+ a b) 2.0))

(define (average-damp f)
  (λ (x) (average x (f x))))

((average-damp (λ (x) (* x x))) 10)

(define (close? a b)
  (< (abs (- a b)) 0.00000001))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-a x)
  (fixed-point (average-damp (λ (y) (/ x y)))
               1.0))
(sqrt 2)

(sqrt-a 2)

(define (cube-root x)
  (fixed-point (average-damp (λ (y) (/ x (* y y))))
               1.0))

(define dx 0.00000001)

(define (deriv g)
  (λ (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (newton-transform g)
  (λ (x)
    (- x(/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))


(define (sqrt-n x)
  (newton-method (λ (y) (- (square y) x))
                 1.0))

(sqrt-n 2)
