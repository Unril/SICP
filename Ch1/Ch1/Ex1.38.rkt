#lang racket

(define (cont-frac get-n get-d k)
  (define (iter curr i)
    (if (= 0 i)
        curr
        (let ((n (get-n i))
              (d (get-d i)))
          (iter (/ n (+ d curr)) (- i 1)))))
  (iter 0.0 k))

(define (cont-frac-r get-n get-d k)
  (define (calc i)
    (let ((n (get-n i))
          (d (get-d i)))
      (if (= i k)
          (/ n d)
          (/ n (+ d (calc (+ i 1)))))))
  (calc 1))

(define (get-d i) 
  (if (= (remainder (+ i 1) 3) 0)
      (* (/ (+ i 1) 3) 2)
      1))

(define ee (cont-frac 
            (λ (i) 1.0)
            get-d 
            10))

(define ee-r (cont-frac-r 
              (λ (i) 1.0)
              get-d 
              10))

(exp 1)
(+ 2 ee)
(+ 2 ee-r)