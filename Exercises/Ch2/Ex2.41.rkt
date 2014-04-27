#lang racket

(require "../my-lib.rkt")

{define (find-three end sum)
  (filter {λ (three)
            (= sum (apply + three))}
          (unique-three 1 end))}


(find-three 10 12)