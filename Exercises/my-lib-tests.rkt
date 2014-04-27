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
