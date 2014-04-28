#lang racket

{define (transpose items) (apply map {λ col col} items)}

{define (get r c items)
  (list-ref (list-ref items r) c)}

{define (diag items )
  (define len (length items))
  (map {λ (p) 
         (map {λ (q)
                (get (sub1 (- len q)) (- p q) items)}
              (range (min p (sub1 len)) (sub1 (max 0 (add1 (- p len)))) -1))}       
       (range (+ len len -1)))}

{define (diag1 positions )
  (diag positions)}

{define (diag2 positions)
  (diag (reverse positions))}

{define (safe? k positions board-size)
  {define (sum-eq-1 items) (<= (apply + items) 1)}
  (and (andmap sum-eq-1 (transpose positions))
       (andmap sum-eq-1 positions)
       (andmap sum-eq-1 (diag1 positions))
       (andmap sum-eq-1 (diag2 positions)))}

{define (rng board-size) (range 1 (add1 board-size))}

{define (adjoin-position new-row k rest-of-queens board-size)
  (map {λ (row row-num) 
         (map {λ (cell col-num)
                (if (and (= col-num k) (= row-num new-row)) 1 cell)}
              row 
              (rng board-size))}
       rest-of-queens
       (rng board-size))}

{define (empty-board board-size)
  (build-list board-size 
              {λ (n)
                (make-list board-size 0)})}

{define (queens board-size)
  {define (queen-cols k)
    (if (= k 0)
        (list (empty-board board-size))
        (filter
         {λ (positions) 
           (safe? k positions board-size)}
         (append-map
          {λ (rest-of-queens)
            (map {λ (new-row)
                   (adjoin-position new-row k rest-of-queens board-size)}
                 (rng board-size))}
          (queen-cols (sub1 k)))))}
  (queen-cols board-size)}

{define (disp items) (for-each {λ (item) (display item) (newline)} items)}

(define q (take (queens 8) 5))
(for-each {λ (item) (disp item) (newline)} q)