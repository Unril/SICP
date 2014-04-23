#lang racket

{define (square-tree func tree)
  (cond [(null? tree) null]
        [(not (pair? tree)) (func tree)]
        [else (cons (square-tree func (car tree))
                    (square-tree func (cdr tree)))])}

{define (square-tree-m func tree)
  (map (λ (sub) 
         (if (pair? sub)
             (square-tree-m func sub)
             (func sub)))
       tree)}

(define ls
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

{define (square x)
  (* x x)}

(square-tree square ls)
(square-tree-m square ls)