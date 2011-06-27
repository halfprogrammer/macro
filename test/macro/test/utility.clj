(ns macro.test.utility
  (:use [macro.utility])
  (:use [clojure.test]))

(deftest replace-me
  (testing "find2"
   (let [lst '(one two three)]
     (is (= ['one true] (find2 #(= % 'one) lst)))
     (is (= ['two true] (find2 #(= % 'two) lst)))
     (is (= ['three true] (find2 #(= % 'three) lst)))
     (is (= nil (find2 #(= % 'four) lst))))))