#lang racket

{define (count-leaves tree)
  (apply + 
         (map {λ (sub) 
                (cond [(null? sub) 0]
                      [(not (pair? sub)) 1]
                      [else (count-leaves sub)])}
              tree))}

(count-leaves (list (list 1 1 1) 1 (list (list 1 1) 1))) ; 7