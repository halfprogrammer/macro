(ns macro.test.utility
  (:use [macro.utility])
  (:use [clojure.test]))

(deftest test-find2
  (testing "find2"
   (let [lst '(one two three)]
     (is (= ['one true lst] (find2 #(= % 'one) lst)))
     (is (= ['two true (rest lst)] (find2 #(= % 'two) lst)))
     (is (= ['three true (nthnext lst 2)] (find2 #(= % 'three) lst)))
     (is (= nil (find2 #(= % 'four) lst))))))

(deftest test-single
  (is (single? '(hello)))
  (is (not (single? ())))
  (is (not (single? (range 10)))))


(deftest test-append
  (is (= '(1) (append () 1)))
  (is (= '(1 2) (append '(1) 2))))


(deftest test-mklist
  (is (= '(1) (mklist 1)))
  (is (= '(1) (mklist '(1))))
  (is (= '(nil) (mklist nil)))
  (is (= () (mklist ()))))

(deftest test-longer?
  (testing "longer? collection"
    (is (longer? (range 10) (range 7)))
    (is (longer? (range 10) (range 0)))
    (is (not (longer? (range 7) (range 10))))
    (is (not (longer? (range 0) (range 10))))))


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

(deftest test-my-flatten
  (is (= '(0 1 2) (my-flatten(range 3))))
  (is (= '() (my-flatten ())))
  (is (= '() (my-flatten '(() ()))))
  (is (= '(1 2 3 4) (my-flatten '(1 (2 (3) 4))))))

(deftest test-prune
  (is (= (range 5) (prune (range 5) (constantly false))))
  (is (= () (prune (range 5) (constantly true))))
  (is (= (range 0 10 2) (prune (range 10) odd?))))

(deftest test-before
  (testing "valid lists"
    (let [lst (range 10)]
      (is (before lst 1 2))
      (is (before lst 1 100))
      (is (not (before lst 2 1)))
      (is (not (before lst 20 50)))))
  (testing "invalid lists"
    (is (not (before 53 20 50)))))


(deftest test-after
  (testing "valid lists"
    (let [lst (range 10)]
      (is (not (after lst 1 2)))
      (is (not (after lst 1 100)))
      (is (after lst 2 1)))))


(deftest test-duplicate?
  (is (falsey? (duplicate? (range 5) 2)))
  (is (duplicate? (list 1 2 2 3 4) 2))
  (is (duplicate? (range 5) 1 #(rem % 2))))

(deftest test-split-if)