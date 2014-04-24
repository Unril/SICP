#lang racket

{define (filter predicate? sequence)
  (cond [(null? sequence) null]
        [(predicate? (car sequence))
         (cons (car sequence)
               (filter predicate? (cdr sequence)))]
        [else (filter predicate? (cdr sequence))])}

{define (accumulate operation initial sequence)
  (if (null? sequence)
      initial
      (operation (car sequence)
                 (accumulate operation initial (cdr sequence))))}

{define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (add1 low) high)))}

{define (enumerate-tree tree)
  (cond [(null? tree) null]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))])}