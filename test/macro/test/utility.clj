(ns macro.test.utility
  (:use [macro.utility])
  (:use [clojure.test]))

(deftest test-find2
  (testing "find2"
   (let [lst '(one two three)]
     (is (= ['one true] (find2 #(= % 'one) lst)))
     (is (= ['two true] (find2 #(= % 'two) lst)))
     (is (= ['three true] (find2 #(= % 'three) lst)))
     (is (= nil (find2 #(= % 'four) lst))))))

(deftest test-single
  (is (= true (truthy? (single? '(hello)))))
  (is (= true (falsey? (single? ()))))
  (is (= true (falsey? (single? (range 10))))))


(deftest test-append
  (is (= '(1) (append () 1)))
  (is (= '(1 2) (append '(1) 2))))


(deftest test-mklist
  (is (= '(1) (mklist 1)))
  (is (= '(1) (mklist '(1))))
  (is (= '(nil) (mklist nil)))
  (is (= () (mklist ()))))

(deftest test-longer
  (testing "longer collection"
    (is (= true (longer (range 10) (range 7))))
    (is (= true (longer (range 10) (range 0))))
    (is (= false (longer (range 7) (range 10))))
    (is (= false (longer (range 0) (range 10))))))


(deftest test-my-filter
  (is (= '(1 3 5) (my-filter odd? (range 6))))
  (is (= () (my-filter #(> % 10) (range 10))))
  (is (= () (my-filter #(> % 10) ()))))

(deftest test-my-group
  (testing "valid-my-group"
    (is (= '((0 1) (2 3)) (my-group (range 4) 2)))
    (is (= '((0 1) (2 3) (4)) (my-group (range 5) 2)))
    (is (= '((0 1 2)) (my-group (range 3) 5)))
    (is (= '(()) (my-group () 5))))

  (testing "invalid my-group"
    (is (thrown? IllegalArgumentException (my-group (range 10) 0)))
    (is (thrown? IllegalArgumentException (my-group (range 10) -4)))))