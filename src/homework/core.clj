(ns homework.core)

(defn find-node [predicate tree]
  (if (predicate tree)
    tree
    (first (filter #(find-node predicate %) (:children tree)))))

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


(defn find-node-with-path [path-so-far path-predicate get-children tree]
  (let [new-path (conj path-so-far tree)]
    (if (path-predicate new-path)
    tree
    (some
      #(find-node-with-path
         new-path
         path-predicate
         get-children
         %)
      (get-children tree)))))
;CONTRACT
;if it's in the subree
  ;return it
;if it's not in the subtee
  ;return nil

;IDEA:
; if predicate tree
;  tree
;  else
; get the children of the tree and iterate through them, checking each one
