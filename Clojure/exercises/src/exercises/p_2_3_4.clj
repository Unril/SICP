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

(defn make-code-tree
  [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(deftest three-tests
  (let [lf
        (make-leaf 'a 1)
        t
        (make-code-tree lf
                        (make-leaf 'b 2))]
    (testing "Should get symbols for a leaf"
      (is (= '(a) (symbols lf))))
    (testing "Should get symbols for a three"
      (is (= '(a b) (symbols t))))
    (testing "Should get weight for a leaf"
      (is (= 1 (weight lf))))
    (testing "Should get weight for a three"
      (is (= 3 (weight t))))))

