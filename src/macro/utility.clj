(ns macro.utility
  (:require [swank.core]))

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

(defn prune [coll f]
  (letfn [(rec [tree acc]
            (cond (empty? tree) (reverse acc)
                  (seq? (first tree)) (rec (rest tree)
                                           (conj acc (rec (first tree) ())))
                  :t (rec (rest tree)
                          (if (f (first tree))
                            acc
                            (conj acc (first tree))))))]
    (rec coll ())))


;;; Search

(defn find2 [f coll]
  (if (empty? (seq coll))
    nil
    (if-let [val (f (first coll))]
      [(first coll) val coll]
      (recur f (rest coll)))))


(defn before [lst x y & {:keys [test] :or {test =}}]
  (if (seq? lst)
    (let [f (first lst)]
      (cond (test y f) false
            (test x f) lst
            :else (recur (next lst) x y {:test test})))))


(defn after [lst x y & {:keys [test] :or {test =}}]
  ;; (println "istest= " (= test =))
  (let [rst (before lst y x :test test)]
    (when rst
      (find2 (partial test x) (rest rst)))))


(defn duplicate? [lst x & {:keys [test] :or {test =}}]
  (let [func (partial test x)
        [_ _ rem] (find2 func lst)]
    (if rem
      (truthy? (find2 func (rest rem))))))


(defn split-if [f coll]
  (loop [lst coll
         acc (transient [])]
    (if (or (empty? (seq lst))
            (not (f (first lst))))
      [(or (seq (persistent! acc)) ()) lst]
      (recur (rest lst)
             (conj! acc (first lst))))))