#lang racket

(define (square x) (* x x))
(define (aver a b) (/ (+ a b) 2.0))

(define (iter-impr good? improve)
  (λ (guess x)
    (define (iter current-guess)
      (if (good? current-guess x)
          current-guess
          (iter (improve current-guess x))))
    (iter guess)))

(define sqrt-i (iter-impr
                (λ (guess x)
                  (< (abs (- (square guess) x)) 0.000001)) 
                (λ (guess x)
                  (aver guess (/ x guess)))))

(sqrt-i 1.0 2.0)
(sqrt 2.0)