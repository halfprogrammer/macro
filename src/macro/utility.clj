(ns macro.utility)

(defn find2 [f coll]
  (if (empty? (seq coll))
    nil
    (if-let [val (f (first coll))]
      [(first coll) val]
      (recur f (rest coll)))))


(defn truthy? [x]
  (if x
    true
    false))

(defn falsey? [x]
  (not (truthy? x)))


;;; Operations on Lists
(defn single? [lst]
  (and (list? lst) (first lst) (nil? (next lst))))

(defn append [lst obj]
  (concat lst (list obj)))

(defn mklist [obj]
  (if (list? obj)
    obj
    (list obj)))


(defn longer? [x y]
  (letfn [(compare [x y]
            (and (coll? x)
                 (or (nil? y)
                     (recur (next x) (next y)))))]
    (if (and (coll? x) (coll? y))
      (compare x y)
      (> (count x) (count y)))))


(defn my-filter [f lst]
  (let [acc (transient [])]
    (doseq [x lst :when (f x)]
      (conj! acc x))
    (or (seq (persistent! acc))
        ())))


(defn my-group [coll n]
  (if (<= n 0) (throw (IllegalArgumentException. "'n' should be greater than 0")))

  (letfn [(rec [src acc]
            (if-let [nxt (nthnext src n)]
              (rec nxt (conj! acc (take n src)))
              (seq (persistent! (conj! acc src)))))]
    (if coll
      (rec coll (transient []))
      ())))

(defn atomic? [x]
  (not (coll? x)))

(defn my-flatten [lst]
  (letfn [(rec [x acc]
            (cond (atomic? x) (conj acc x)
                  (empty? x) acc
                  :else (rec (first x) (rec (rest x) acc))))]
    
    (rec lst ())))

;; (defn prune [coll f]
;;   (letfn [(rec [tree acc]
;;             (cond (empty? tree) (reverse acc)
;;                   (list? (first tree)) (rec (rest tree)
;;                                             (conj acc (rec (first tree) ())))
;;                   :t (rec (rest tree)
;;                           (if (f (first tree))
;;                             acc
;;                             (conj acc (first tree))))))]
;;     (rec coll ())))