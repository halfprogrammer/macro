(ns macro.utility)

(defn find2 [f coll]
  (if (empty? (seq coll))
    nil
    (if-let [val (f (first coll))]
      [(first coll) val]
      (recur f (rest coll)))))