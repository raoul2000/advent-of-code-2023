(ns day-7-test
  (:require [clojure.test :refer [deftest testing are is]]
            [day-7 :as d]))


(deftest card-comparator-test
  (testing "compare 2 single card"
    (let [compare-cards (d/create-cards-comparator [\a \b \b])]
      (is (=  0 (compare-cards \a \a)))
      (is (= -1 (compare-cards \a \b)))
      (is (=  1 (compare-cards \b \a))))))


(deftest cards-hand-comparator-test
  (testing "compare cards hands"
    (are [expected args]  (= expected (d/compare-hands (first args) (second args)))
      0   ["A" "A"]
      -1  ["K" "A"])))