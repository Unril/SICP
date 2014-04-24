#lang racket

{define (rev-1 seq) 
  (foldr {λ (item rest) (append rest (list item))} null seq)}

{define (rev-2 seq) 
  (foldl cons null seq)}

{rev-1 (list 1 2 3)}
{rev-2 (list 1 2 3)}