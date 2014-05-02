#lang racket

{define (memq item items)
  (cond [(null? items) #f]
        [(eq? item (car items)) items]
        [else (memq item (cdr items))])}

(list 'a 'b 'c)
(list (list 'test))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a b c)))
(memq 'red '((red a) (b c)))
(memq 'red '(red a b c))
(memq 'red '(a b red c))