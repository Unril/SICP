#lang racket

(define (aver a b)
  (/ (+ a b) 2))

(define (close? a b)
  (< (abs (- a b)) 0.0001))

(define (search f neg-p pos-p)
  (let ((mid-p (aver neg-p pos-p)))
    (if (close? neg-p pos-p)
        mid-p
        (let ((test-val (f mid-p)))
          (cond ((positive? test-val)
                 (search f neg-p mid-p))
                ((negative? test-val)
                 (search f mid-p pos-p))
                (else mid-p))))))

(define (half-interval-method f a b)
  (let ((a-val (f a))
        (b-val (f b)))
    (cond ((and (negative? a-val) (positive? b-val))
           (search f a b))
          ((and (negative? b-val) (positive? a-val))
           (search f b a))
          (else (error "same signs " a b)))))

(half-interval-method sin 2.0 4.0)

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (λ (y) (+ (sin y) (cos y)))
             1.0)

(define (sqrt-f x)
  (fixed-point (λ (y) (aver y (/ x y)))
               1.0))

(sqrt-f 2.0)