(ns homework.core)

(defn find-all-seq [get-children predicate tree]
  (filter
    #(and (predicate %) %)
    (tree-seq get-children get-children tree)))

(defn find-all [predicate tree]
  (set (find-all-seq :children predicate tree)))

(defn find-node-custom [predicate get-children tree]
  (first (find-all-seq get-children predicate tree)))

(defn find-node [predicate tree]
  (find-node-custom predicate :children tree))

(defn add-path [loc]
  (fn [child] {:value child
               :ppath (conj (:ppath loc) child)}))

;; A path seq would be more pleasant than this opaque path-predicate.
(defn find-node-with-path [_ path-predicate get-children tree]
  (let [root-node {:value tree :ppath [tree]}
        children-values (comp get-children :value)
        get-children-with-path (fn [loc]
                                 (map (add-path loc)
                                      (children-values loc)))]
    (:value (find-node-custom (comp path-predicate :ppath)
                              get-children-with-path
                              root-node))))

