(ns day-10-test
  (:require [clojure.test :refer [deftest testing are is]]
            [day-10 :as d]))

(def sample-input-1
  "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
")

(deftest helper-test
  (testing "Complement direction"
    (is (= :north (d/complement-direction :south)))
    (is (= :south (d/complement-direction :north)))
    (is (= :west  (d/complement-direction :east)))
    (is (= :east  (d/complement-direction :west)))
    (is (thrown? Exception (d/complement-direction :north-west))))

  (testing "find matching pipe"
    (is (nil? (d/find-matching-pipes \| :east)))
    (is (= #{\F \7 \|} (d/find-matching-pipes \| :north)))
    (is (nil?  (d/find-matching-pipes \- :north))))

  (testing "adjacent coords"
    (let [grid (d/create-grid sample-input-1)]
      (is (= '([:east [1 0] \-] [:south [0 1] \.])
             (d/adjacent-coords [0 0] grid)))
      (is (= '([:west [3 4] \L] [:north [4 3] \J])
             (d/adjacent-coords [4 4] grid)))
      (is (= '([:east [2 2] \L] [:west [0 2] \S] [:south [1 3] \F] [:north [1 1] \F])
             (d/adjacent-coords [1 2] grid)))))) 
 