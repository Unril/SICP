#lang racket

(provide deriv)

{define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp)))]
        [(exponentiation? exp)
         (let ([u (base exp)]
               [n (exponent exp)])
           (make-product (make-product n (make-exponentiation u (sub1 n))) (deriv u var)))]
        [else (error "Unknown expr type" exp)])}


{define (variable? exp)
  (symbol? exp)}

{define (same-variable? exp1 exp2)
  (and (variable? exp1) (variable? exp2) (eq? exp1 exp2))}

{define (=number? exp num)
  (and (number? exp) (= exp num))}

{define (check operation exp)
  (and (pair? exp) (eq? (car exp) operation))}

{define (get-first exp) (cadr exp)}

{define (get-rest operation exp)
  (let ([rest (cddr exp)])
    (if (null? (cdr rest))
        (car rest) 
        (cons operation rest)))}

; sum
{define (sum? exp) (check '+ exp)}

{define (addend exp) (cadr exp)}

{define (augend exp) (get-rest '+ exp)}

{define (make-sum exp1 exp2)
  (cond [(=number? exp1 0) exp2]
        [(=number? exp2 0) exp1]
        [(and (number? exp1) (number? exp2) (+ exp1 exp2))]
        [else (list '+ exp1 exp2)])}

; product
{define (product? exp) (check '* exp)}

{define (multiplier exp) (get-first exp)}

{define (multiplicand exp) (get-rest '* exp)}

{define (make-product exp1 exp2)
  (cond [(or (=number? exp1 0) (=number? exp2 0)) 0]
        [(=number? exp1 1) exp2]
        [(=number? exp2 1) exp1]
        [(and (number? exp1) (number? exp2) (* exp1 exp2))]
        [else (list '* exp1 exp2)])}


; exponentiation
{define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '^) (number? (exponent exp)))}

{define (base exp) (get-first exp)}
{define (exponent exp) (get-rest '* exp)}

{define (make-exponentiation base-exp exponent-exp)
  (cond [(= exponent-exp 0) 1]
        [(=number? base-exp 0) 0]
        [(= exponent-exp 1) base-exp]
        [(and (number? base-exp) (expt base-exp exponent-exp))]
        [else (list '^ base-exp exponent-exp)])}






