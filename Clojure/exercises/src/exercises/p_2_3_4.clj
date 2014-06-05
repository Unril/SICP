(ns exercises.p-2-3-4
  (:require [clojure.test :refer :all]))

(defn make-leaf
  [symbol weight]
  (list :leaf symbol weight))

(defn leaf?
  [leaf]
  (= :leaf (first leaf)))

(defn symbol-leaf
  [leaf]
  (second leaf))

(defn weight-leaf
  [leaf]
  (nth leaf 2))

(deftest leaf-tests
  (let [lf
        (make-leaf 'a 2)]
    (testing "Should get parts of the leaf"
      (is (= 'a (symbol-leaf lf)))
      (is (= 2 (weight-leaf lf))))
    (testing "Should check is it a leaf"
      (is (leaf? lf))
      (is (not (leaf? (list 1 2 3)))))))

(defn left-branch
  [tree]
  (first tree))

(defn right-branch
  [tree]
  (second tree))

(defn symbols
  [three]
  (if (leaf? three)
    (list (symbol-leaf three))
    (nth three 2)))

(defn weight
  [three]
  (if (leaf? three)
    (weight-leaf three)
    (nth three 3)))

(defn make-tree
  [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(deftest tree-tests
  (let [leaf (make-leaf 'a 1)
        tree (make-tree leaf (make-leaf 'b 2))]
    (testing "Should get symbols for a leaf"
      (is (= '(a) (symbols leaf))))
    (testing "Should get symbols for a three"
      (is (= '(a b) (symbols tree))))
    (testing "Should get weight for a leaf"
      (is (= 1 (weight leaf))))
    (testing "Should get weight for a three"
      (is (= 3 (weight tree))))))

(defn choose-branch
  [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else (throw (IllegalStateException. "Wrong bit!"))))

(declare decode-impl)
(defn decode
  [bits tree]
  (defn decode-impl
    [bits current-branch]
    (if (empty? bits)
      '()
      (let [next-branch (choose-branch (first bits) current-branch)]
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-impl (rest bits) tree))
          (decode-impl (rest bits) next-branch)))))
  (decode-impl bits tree))

(deftest decode-tests
  (let [subtree (make-tree (make-leaf 'a 0) (make-leaf 'b 0))
        tree (make-tree subtree (make-leaf 'c 0))]
    (testing "Should return empty list when pass empty bits"
      (is (= '() (decode '() subtree))))
    (testing "Should decode using subtree"
      (is (= '(a a b a b) (decode '(0 0 1 0 1) subtree))))
    (testing "Should decode using tree"
      (is (= '(b) (decode '(0 1) tree)))
      (is (= '(a) (decode '(0 0) tree)))
      (is (= '(c) (decode '(1) tree)))
      (is (= '(c a b a c) (decode '(1 0 0 0 1 0 0 1) tree))))))

(defn adjoin-set
  [x set]
  (cond (empty? set) (list x)
        (< (weight x) (weight (first set))) (cons x set)
        :else (cons (first set) (adjoin-set x (rest set)))))

(deftest adjoin-set-tests
  (let [lf-1 (make-leaf 'a 1)
        lf-2 (make-leaf 'b 2)
        lf-3 (make-leaf 'c 3)]
    (testing "Should add item to empty set"
      (is (= (list lf-1) (adjoin-set lf-1 '()))))
    (testing "Should add item to set without this item"
      (is (= (list lf-1 lf-2) (adjoin-set lf-1 (list lf-2))))
      (is (= (list lf-1 lf-2) (adjoin-set lf-2 (list lf-1))))
      (is (= (list lf-1 lf-2 lf-3) (adjoin-set lf-1 (list lf-2 lf-3))))
      (is (= (list lf-1 lf-2 lf-3) (adjoin-set lf-2 (list lf-1 lf-3)))))))

(defn make-leaf-set
  [pairs]
  (if (empty? pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair) (second pair))
                  (make-leaf-set (rest pairs))))))

(deftest make-leaf-set-tests
  (let [pairs (list '(a 2) '(b 1) '(c 3))]
    (testing "Should get empty set when pass empty set"
      (is (= '() (make-leaf-set '()))))
    (testing "Should create orgered set of leafs"
      (is (= '((:leaf b 1) (:leaf a 2) (:leaf c 3)) (make-leaf-set pairs))))))


; exercise 2.67

(def sample-tree
  (make-tree (make-leaf 'a 4)
             (make-tree (make-leaf 'b 2)
                        (make-tree (make-leaf 'd 1)
                                   (make-leaf 'c 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(def sample-data '(a d a b b c a))

(deftest decode-sample-message-tests
  (testing "Should decode sample message"
    (is (= sample-data (decode sample-message sample-tree)))))


; exercise 2.68


(defn check-symbol
  [tree symbol]
  (some #(= % symbol) (symbols tree)))

(defn encode-symbol
  [symbol tree result]
  (let [lb (left-branch tree)
        rb (right-branch tree)]
    (cond (leaf? tree) result
          (check-symbol lb symbol) (encode-symbol symbol lb (concat result '(0)))
          (check-symbol rb symbol) (encode-symbol symbol rb (concat result '(1)))
          :else (throw (IllegalStateException. "Symbol not found!")))))

(defn encode
  [data tree]
  (if (empty? data)
    '()
    (concat (encode-symbol (first data) tree '())
            (encode (rest data) tree))))

(deftest encode-tests
  (testing "Should check symbol"
    (is (check-symbol sample-tree 'a))
    (is (check-symbol sample-tree 'b))
    (is (check-symbol (make-leaf 'a 0) 'a)))
  (testing "Should encode symbol"
    (is (= '(0) (encode-symbol 'a sample-tree '())))
    (is (= '(1 0) (encode-symbol 'b sample-tree '())))
    (is (= '(1 1 1) (encode-symbol 'c sample-tree '())))
    (is (= '(1 1 0) (encode-symbol 'd sample-tree '()))))
  (testing "Should encode sample message"
    (is (= sample-message (encode sample-data sample-tree)))))

