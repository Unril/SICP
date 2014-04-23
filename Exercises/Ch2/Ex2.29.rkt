#lang racket

; Бинарный мобиль состоит из двух ветвей, левой и правой. Каждая ветвь представляет собой
; стержень определенной длины, с которого свисает либо гирька, либо еще один бинарный мобиль.

(define (mob left right)
  (list left right))

; Ветвь составляется из длины length (которая должна быть числом) и структуры structure,
; которая может быть либо числом (представляющим простую гирьку), либо еще одним мобилем:

(define (br len struct)
  (list len struct))

(define test-bal 
  (mob (br 20 
           (mob (br 20 1)
                (br 10 2)))
       (br 10 6)))

; Напишите соответствующие селекторы left-branch и right-branch, которые возвращают левую и
; правую ветви мобиля, а также branch-length и branch-structure, которые возвращают компоненты ветви.

(define (left-br m)
  (list-ref m 0))

(define (right-br m)
  (list-ref m 1))

(define (br-len b)
  (list-ref b 0))

(define (br-structure b)
  (list-ref b 1))

(define (simple? b)
  (not (list? (br-structure b))))

(define (complex? b)
  (list? (br-structure b)))

; С помощью этих селекторов напишите процедуру total-weight, которая возвращает общий вес мобиля.

(define (total-weight m) 
  (define (branch-weight b)
    (if (simple? b)
        (br-structure b)
        (total-weight (br-structure b))))
  (+ (branch-weight (left-br m))
     (branch-weight (right-br m))))

; Говорят, что мобиль сбалансирован, если момент вращения, действующий на его левую ветвь,
; равен моменту вращения, действующему на правую ветвь (то есть длина левого стержня, умноженная
; на вес груза, свисающего с него, равна соответствующему произведению для правой стороны),
; и если все подмобили, свисающие с его ветвей, также сбалансированы. Напишите предикат, который 
; проверяет мобили на сбалансированность.

(define (balanced? m) 
  (define (momentum br)
    (* (br-len br) 
       (if (simple? br)
           (br-structure br)
           (total-weight (br-structure br)))))
  
  (define (balanced-br? br)
    (or (simple? br) 
        (balanced? (br-structure br))))
  
  (let ([l (left-br m)]
        [r (right-br m)])
    (and (= (momentum l) (momentum r))
         (balanced-br? l)
         (balanced-br? r))))
'true
(balanced? test-bal)

'false
(balanced? (mob (br 1 1) 
                (br 2 2))) 

'true
(balanced? (mob (br 1 4)
                (br 2 2)))
'-
'false
(balanced? (mob (br 1 (mob (br 1 1)
                           (br 1 1)))
                (br 2 2)))

'true
(balanced? (mob (br 2 (mob (br 2 1)
                           (br 1 2)))
                (br 1 6)))
'-
'false
(balanced? (mob (br 1 (mob (br 1 1)
                           (br 1 1)))
                (br 1 (mob (br 1 1)
                           (br 1 2)))))

'true
(balanced? (mob (br 1 (mob (br 1 1)
                           (br 1 1)))
                (br 1 (mob (br 1 1)
                           (br 1 1)))))


