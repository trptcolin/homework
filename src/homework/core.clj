(ns homework.core)

(declare find-node-custom)

(defn find-node [predicate tree]
  (find-node-custom predicate #(:children %) tree))

(defn find-all [predicate tree]
  (let [new-finds (if (predicate tree)
                    #{tree}
                    #{})]
    (reduce
      clojure.set/union
      new-finds
      (map #(find-all predicate %) (:children tree)))))

(defn find-node-custom [predicate get-children tree]
  (if (predicate tree)
    tree
    (some
      #(find-node-custom predicate get-children %)
      (get-children tree))))


(defn find-node-with-path [_ path-predicate get-children tree]
  (let [get-children-metagraph (fn [node]
                                 (vec (map
                                        (fn [child] {:node child :path (conj (vec (:path node)) child)})
                                        (get-children (:node node)))))
        metagraph-predicate #(path-predicate (:path %))]
    (:node (find-node-custom metagraph-predicate get-children-metagraph {:node tree :path [tree]}))))
