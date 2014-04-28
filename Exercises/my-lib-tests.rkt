#lang racket

(require rackunit
         "my-lib.rkt")

(test-case
 "Behaviour of the unique-pairs function."
 (check-equal? 0
               (length (unique-pairs 1 0))
               "Should return emtpy list when end is lower than start.")
 (check-equal? 0 
               (length (unique-pairs 1 1))
               "Should return emtpy list when end is equal start.")
 (check-equal? (list (list 2 1))
               (unique-pairs 1 2)
               "Should return one pair (i=end, j=start) for range [1, 2].")
 (check-equal? (list (list 2 1) (list 3 1) (list 3 2))
               (unique-pairs 1 3)
               "Should generate pairs for range [1, 3]."))


(test-case
 "2D vector operations"
 (let ([v1 (vect 1 2)]
       [v2 (vect 3 5)])
   (check-equal? 1
                 (vect-x v1)
                 "Should get x coord")
   (check-equal? 2
                 (vect-y v1)
                 "Should get y coord")
   (check-equal? (vect 4 7)
                 (vect-add v1 v2)
                 "Should get add vectors")
   (check-equal? (vect 2 3)
                 (vect-sub v2 v1)
                 "Should get substract vectors")
   (check-equal? (vect 2 4)
                 (vect-scale v1 2)
                 "Should scale vector")))