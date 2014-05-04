#lang racket

; unordered set
(provide element-of-uset?
         adjoin-uset
         intersection-uset
         union-uset)

{define (element-of-uset? x set)
  (list? (member x set))}

{define (adjoin-uset x set)
  (if (element-of-uset? x set)
      set
      (cons x set))}

{define (intersection-uset set1 set2)
  (filter {λ (x)
            (element-of-uset? x set1)}
          set2)}

{define (union-uset set1 set2)
  (append set1
          (filter {λ (x)
                    (not (element-of-uset? x set1))}
                  set2))}

; ordered set
(provide element-of-oset?
         intersection-oset
         adjoin-oset
         union-oset)

{define (element-of-oset? x set)
  (cond [(null? set) #f]
        [(= x (car set)) #t]
        [(< x (car set)) #f]
        [else (element-of-oset? x (cdr set))])}

{define (intersection-oset set1 set2)
  (if (or (null? set1) (null? set2))
      null
      (let ([x1 (car set1)]
            [x2 (car set2)])
        (cond [(= x1 x2)
               (cons x1 (intersection-oset (cdr set1) (cdr set2)))]
              [(< x1 x2)
               (intersection-oset (cdr set1) set2)]
              [else 
               (intersection-oset set1 (cdr set2))])))}

{define (adjoin-oset x set)
  (cond [(null? set) (list x)]
        [(= x (car set)) set]
        [(< x (car set)) (cons x set)]
        [else (cons (car set) (adjoin-oset x (cdr set)))])}

{define (union-oset set1 set2)
  (cond [(and (null? set1) (null? set2)) null] 
        [(null? set1) set2]
        [(null? set2) set1]
        [else (let ([x1 (car set1)]
                    [x2 (car set2)])
                (cond [(= x1 x2) 
                       (cons x1 (union-oset (cdr set1) (cdr set2)))]
                      [(< x1 x2) 
                       (cons x1 (union-oset (cdr set1) set2))]
                      [(> x1 x2) 
                       (cons x2 (union-oset set1 (cdr set2)))]
                      ))])}

; tree set
(provide entry left-br right-br make-tree
         element-of-tset?
         tree->list
         list->tree
         print-tree
         union-tset
         intersection-tset)

{define (entry tree) (first tree)}
{define (left-br tree) (second tree)}
{define (right-br tree) (third tree)}
{define (make-tree entry left right) (list entry left right)}

{define (element-of-tset? x set)
  (if (null? set) #f
      (let ([e (entry set)])
        (cond [(= x e) #t]
              [(< x e)
               (element-of-tset? x (left-br set))]
              [else 
               (element-of-tset? x (right-br set))])))}

{define (tree->list tree)
  (if (null? tree) null
      (append (tree->list (left-br tree))
              (cons (entry tree)
                    (tree->list (right-br tree)))))}

{define (list->tree elements)
  (car (partial-tree elements (length elements)))}

{define (partial-tree el n)
  (if (= n 0)
      (cons null el)
      (let ([left-size (quotient (- n 1) 2)])
        (let ([left-result (partial-tree el left-size)])
          (let ([left-tree (car left-result)]
                [non-left-el (cdr left-result)]
                [right-size (- n (+ left-size 1))])
            (let ([this-entry (car non-left-el)]
                  [right-result (partial-tree (cdr non-left-el) right-size)])
              (let ([right-tree (car right-result)]
                    [remaining-el (cdr right-result)])
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-el)))))))}

{define (print-tree tree)
  {define (print-impl tree indent)
    (cond [(null? tree) (void)]
          [else
           (printf "~a~a~n" indent (entry tree))
           (print-impl (left-br tree) (format "  ~a" indent)) 
           (print-impl (right-br tree) (format "  ~a" indent))])}
  (print-impl tree "")}

{define (union-tset t1 t2)
  (list->tree (union-oset (tree->list t1) (tree->list t2)))}


{define (intersection-tset t1 t2)
  (list->tree (intersection-oset (tree->list t1) (tree->list t2)))}


















