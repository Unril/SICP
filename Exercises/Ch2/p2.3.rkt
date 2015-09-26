#lang racket
(require rackunit)

'----------memq

(define (memq item items)
  (cond ((null? items) #f)
        ((eq? (car items) item) items)
        (else (memq item (cdr items)))))

(check-equal? (memq 'a '()) #f)
(check-equal? (memq 'a '(a b)) '(a b))
(check-equal? (memq 'a '(c d)) #f)
(check-equal? (memq 'a '(b a c)) '(a c))

'----------equal?

(define (equal-1? ls1 ls2)
  (cond ((and (null? ls1) (null? ls2)) #t)
        ((and (pair? ls1) (pair? ls2) (eq? (car ls1) (car ls2))) (equal-1? (cdr ls1) (cdr ls2)))
        ((and (symbol? ls1) (symbol? ls2)) (eq? ls1 ls2))
        (else #f)))

(define (compare-eq ls1 ls2)
  (check-equal? (equal-1? ls1 ls2) (equal? ls1 ls2)))

(compare-eq '(mcons 1 2) '(mcons 1 2))
(compare-eq '(mcons 1 2) '(mcons 1 2 3))
(compare-eq '() '())
(compare-eq 'a 'a)
(compare-eq 'b 'a)

'----------derivatives

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (first-el exp) var)
                   (deriv (second-el exp) var)))
        ((product? exp)
         (make-sum (make-product (first-el exp) (deriv (second-el exp) var))
                   (make-product (second-el exp) (deriv (first-el exp) var))))
        (else (error "Wrong expression " exp))))


(define (variable? exp)
  (symbol? exp))

(define (same-variable? exp var)
  (and (variable? exp) (variable? var) (eq? exp var)))

(define (sum? exp)
  (and (pair? exp) (eq? '+ (car exp))))

(define (make-sum a b)
  (list '+ a b))

(define (product? exp)
  (and (pair? exp) (eq? '* (car exp))))

(define (make-product a b)
  (list '* a b))

(define (first-el exp)
  (cadr exp))

(define (second-el exp)
  (caddr exp))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(define (simpl exp)
  (cond ((sum? exp) (simple-sum (first-el exp) (second-el exp)))
        ((product? exp) (simple-product (first-el exp) (second-el exp)))
        (else exp)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (simple-sum a b)
  (cond ((=number? a 0) (simpl b))
        ((=number? b 0) (simpl a))
        ((and (number? a) (number? b)) (+ a b))
        (else (list '+ (simpl a) (simpl b)))))

(define (simple-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) (simpl b))
        ((=number? b 1) (simpl a))
        ((and (number? a) (number? b)) (* a b))
        (else (list '* (simpl a) (simpl b)))))

(define (simplify exp)
  (let ([s (simpl exp)])
    (if (equal? s exp) exp (simplify s))))

(define (deriv-s exp var)
  (simplify (deriv exp var)))

(deriv-s '(+ x 3) 'x)
(deriv-s '(* x y) 'x)
(deriv-s '(* (* x y) (+ x 3)) 'x)

'----------unordered-set

(define (element-of-uset? el set)
  (cond ((null? set) #f)
        ((equal? (car set) el) #t)
        (else (element-of-uset? el (cdr set)))))

(define (adjoin-uset el set)
  (if (element-of-uset? el set) set (cons el set)))

(check-equal? (adjoin-uset 'a '(b c)) '(a b c))
(check-equal? (adjoin-uset 'a '(b a c)) '(b a c))
(check-equal? (adjoin-uset 'a '()) '(a))

(define (intersection-uset s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-uset? (car s1) s2) (cons (car s1) (intersection-uset (cdr s1) s2)))
        (else (intersection-uset (cdr s1) s2))))

(check-equal? (intersection-uset '(a b c) '(x y z)) '())
(check-equal? (intersection-uset '(a b c) '(a b c)) '(a b c))
(check-equal? (intersection-uset '(a b c) '(x a y b z)) '(a b))

(define (union-uset s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-uset? (car s1) s2) (union-uset (cdr s1) s2))
        (else (cons (car s1) (union-uset (cdr s1) s2)))))

(check-equal? (union-uset '(a b c) '(x y z)) '(a b c x y z))
(check-equal? (union-uset '(a b c) '(a b c)) '(a b c))
(check-equal? (union-uset '(a b c) '(x a y b z)) '(c x a y b z))

'----------ordered-set
; increasing order (1 3 10)

(define (element-of-oset? el set)
  (cond ((null? set) #f)
        ((= el (car set)) #t)
        ((< el (car set)) #f)
        (else (element-of-oset? el (cdr set)))))

(check-equal? (element-of-oset? 5 '()) #f)
(check-equal? (element-of-oset? 5 '(1 3 6)) #f)
(check-equal? (element-of-oset? 5 '(1 3 5 6)) #t)

(define (adjoin-oset el set)
  (cond ((null? set) (list el))
        ((= el (car set)) set)
        ((> el (car set)) (cons (car set) (adjoin-oset el (cdr set))))
        (else (cons el set))))

(check-equal? (adjoin-oset 5 '()) '(5))
(check-equal? (adjoin-oset 5 '(1 3 6 7)) '(1 3 5 6 7))
(check-equal? (adjoin-oset 5 '(1 3 5 6)) '(1 3 5 6))

(define (intersection-oset s1 s2)
  (if (or (null? s1) (null? s2)) '()
      (let ((x1 (car s1)) (x2 (car s2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-oset (cdr s1) (cdr s2))))
              ((< x1 x2)
               (intersection-oset (cdr s1) s2))
              ((> x1 x2)
               (intersection-oset s1 (cdr s2)))))))

(check-equal? (intersection-oset '(5) '()) '())
(check-equal? (intersection-oset '(2 4) '(1 3 6 7)) '())
(check-equal? (intersection-oset '(3 5) '(1 3 5 6)) '(3 5))

(define (union-oset s1 s2)
  (cond  ((null? s1) s2)
         ((null? s2) s1)
         (else
          (let ((x1 (car s1)) (x2 (car s2)))
            (cond ((= x1 x2)
                   (cons x1 (union-oset (cdr s1) (cdr s2))))
                  ((< x1 x2)
                   (cons x1 (union-oset (cdr s1) s2)))
                  ((> x1 x2)
                   (cons x2 (union-oset s1 (cdr s2)))))))))

(check-equal? (union-oset '(1) '()) '(1))
(check-equal? (union-oset '(2 4) '(1 3 6 7)) '(1 2 3 4 6 7))
(check-equal? (union-oset '(3 5) '(1 3 5 6)) '(1 3 5 6))
(check-equal? (union-oset '(3 5) '(1 5 6)) '(1 3 5 6))

'----------tree-set

(define (node-val node)
  (car node))

(define (node-lbr node)
  (cadr node))

(define (node-rbr node)
  (caddr node))

(define (node val lbr rbr)
  (list val lbr rbr))

(define (leaf val)
  (list val null null))

(define (element-of-tset? el set)
  (cond ((null? set) #f)
        ((= el (node-val set)) #t)
        ((< el (node-val set))
         (element-of-tset? el (node-lbr set)))
        ((> el (node-val set))
         (element-of-tset? el (node-rbr set)))))

(define test-tset (node 7 (node 3 (leaf 1) (leaf 5)) (node 9 null (leaf 11))))

(check-equal? (element-of-tset? 1 test-tset) #t)
(check-equal? (element-of-tset? 7 test-tset) #t)
(check-equal? (element-of-tset? 11 test-tset) #t)
(check-equal? (element-of-tset? 2 test-tset) #f)
(check-equal? (element-of-tset? 22 test-tset) #f)

(define (adjoin-tset el set)
  (cond ((null? set) (leaf el))
        ((= el (node-val set)) set)
        ((< el (node-val set))
         (node (node-val set) (adjoin-tset el (node-lbr set)) (node-rbr set)))
        ((> el (node-val set))
         (node (node-val set) (node-lbr set) (adjoin-tset el (node-rbr set))))))

(check-equal? (adjoin-tset 1 test-tset) test-tset)
(check-equal? (adjoin-tset 11 test-tset) test-tset)
(check-equal? (adjoin-tset 8 test-tset) (node 7 (node 3 (leaf 1) (leaf 5)) (node 9 (leaf 8) (leaf 11))))

(define (tset->list tset)
  (define (to-list tset ls)
    (if (null? tset)
        ls
        (to-list (node-lbr tset)
                 (cons (node-val tset)
                       (to-list (node-rbr tset) ls)))))
  (to-list tset null))

(check-equal? (tset->list test-tset) '(1 3 5 7 9 11))

(define (intersection-tset-list ls tset)
  (cond ((or (null? ls) (null? tset)) null)
        ((element-of-tset? (car ls) tset) (adjoin-tset (car ls) (intersection-tset-list (cdr ls) tset)))
        (else (intersection-tset-list (cdr ls) tset))))

(check-equal? (tset->list (intersection-tset-list '(1 2 7 6 11) test-tset)) '(1 7 11))

(define (union-tset-list ls tset)
  (cond ((null? ls) tset)
        ((null? tset) null)
        (else (adjoin-tset (car ls) (union-tset-list (cdr ls) tset)))))

(check-equal? (tset->list (union-tset-list '(1 2 7 6 11) test-tset)) '(1 2 3 5 6 7 9 11))

'----------

(define (hf-leaf sym weight)
  (list 'leaf sym weight))

(define (hf-leaf? node)
  (eq? (car node) 'leaf))

(define (hf-leaf-symbol leaf)
  (cadr leaf))

(define (hf-leaf-weight leaf)
  (caddr leaf))

(define (hf-tree l r)
  (list l
        r
        (append (hf-symbols l) (hf-symbols r))
        (+ (hf-weight l) (hf-weight r))))

(define (hf-l node)
  (car node))

(define (hf-r node)
  (cadr node))

(define (hf-symbols node)
  (if (hf-leaf? node)
      (list (hf-leaf-symbol node))
      (caddr node)))

(define (hf-weight node)
  (if (hf-leaf? node)
      (hf-leaf-weight node)
      (cadddr node)))

(define (hf-decode bits tree)
  (define (decode bits node)
    (if (null? bits)
        null
        (let ((next-node (hf-choose-branch (car bits) node)))
          (if (hf-leaf? next-node)
              (cons (hf-leaf-symbol next-node)
                    (decode (cdr bits) tree))
              (decode (cdr bits) next-node)))))
  (decode bits tree))

(define (hf-choose-branch bit node)
  (cond ((= 0 bit) (hf-l node))
        ((= 1 bit) (hf-r node))
        (else (error "Wrong bit" bit))))

(define hf-A (hf-leaf 'A 8))
(define hf-B (hf-leaf 'B 3))
(define hf-C (hf-leaf 'C 1))
(define hf-D (hf-leaf 'D 1))
(define hf-E (hf-leaf 'E 1))
(define hf-F (hf-leaf 'F 1))
(define hf-G (hf-leaf 'G 1))
(define hf-H (hf-leaf 'H 1))

(define hf-test-tree (hf-tree hf-A (hf-tree (hf-tree hf-B (hf-tree hf-C hf-D)) (hf-tree (hf-tree hf-E hf-F) (hf-tree hf-G hf-H)))))
(define hf-test-bits '(1 0 0 0 1 0 1 0 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 0 1 0 0 1 0 0 0 0 0 1 1 1 0 0 1 1 1 1))
(define hf-test-message '(B A C A D A E A F A B B A A A G A H))

(check-equal? (hf-decode hf-test-bits hf-test-tree) hf-test-message)

(define (hf-adjoin node set)
  (cond ((null? set) (list node))
        ((< (hf-weight node) (hf-weight (car set))) (cons node set))
        (else (cons (car set) (hf-adjoin node (cdr set))))))

(define (hf-leaf-set pairs)
  (if (null? pairs) null
      (let ((pair (car pairs)))
        (hf-adjoin (hf-leaf (car pair) (cadr pair))
                   (hf-leaf-set (cdr pairs))))))

(define (hf-encode message tree)
  (if (null? message) null
      (append (hf-encode-symbol (car message) tree)
              (hf-encode (cdr message) tree))))

(define (hf-encode-symbol symbol tree)
  (define (encode node bits)
    (if (hf-leaf? node)
        bits
        (let ((l (hf-l node)) (r (hf-r node)))
          (cond ((memq symbol (hf-symbols l)) (encode l (append bits '(0))))
                ((memq symbol (hf-symbols r)) (encode r (append bits '(1))))
                (else (error "Wrong symbol" symbol))))))
  (encode tree '()))

(check-equal? (hf-encode-symbol 'A hf-test-tree) '(0))
(check-equal? (hf-encode-symbol 'B hf-test-tree) '(1 0 0))
(check-equal? (hf-encode-symbol 'G hf-test-tree) '(1 1 1 0))
(check-equal? (hf-encode-symbol 'H hf-test-tree) '(1 1 1 1))
(check-equal? (hf-encode hf-test-message hf-test-tree) hf-test-bits)









