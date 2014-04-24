#lang racket

{define (fold-left op initial sequence)
  {define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest)))}
  (iter initial sequence)}

(foldr + 0 (list 1 2 3))
(fold-left + 0 (list 1 2 3))
(foldr / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(foldr list null (list 1 2 3))
(fold-left list null (list 1 2 3))