#lang racket

{define (square-tree tree)
  (cond [(null? tree) null]
        [(not (pair? tree)) (* tree tree)]
        [else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))])}

{define (square-tree-m tree)
  (map (λ (sub) 
         [if (pair? sub)
             (square-tree-m sub)
             (* sub sub)])
       tree)}

(define ls
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(square-tree ls)
(square-tree-m ls)