(ns exercises.p-2-3-4)

(defn make-leaf
  [symbol weight]
  (list 'leaf symbol weight))

(defn leaf?
  [leaf]
  (= 'leaf (first leaf)))

(defn symbol-leaf
  [leaf]
  (first (rest leaf)))

(defn weight-leaf
  [leaf]
  (first (rest (rest leaf))))