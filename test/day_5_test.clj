(ns day-5-test
  (:require [clojure.test :refer [deftest testing are is]]
            [day-5 :as d]))


(deftest range-comparaison-test
  (testing "included range ?"
    (are [result arg1 arg2]  (= result (d/included? arg1 arg2))
      true  [1 1] [1 1]
      true  [1 2] [1 2]
      true  [1 2] [1 3]
      true  [2 2] [1 3]
      true   [18 24] [18 25]
      false [2 4] [1 3]
      false [1 5] [2 7]
      false [3 8] [2 7]))

  (testing "left overlap range ?"
    (are [result arg1 arg2]  (= result (d/left-overlap? arg1 arg2))
      false  [1 1] [5 9]
      true   [1 5] [5 9]
      true   [1 9] [5 9]
      true   [1 10] [5 9]
      false   [18 24] [18 25]
      false   [5 5] [5 9] ;; included 
      ))

  (testing "right overlap range ?"
    (are [result arg1 arg2]  (= result (d/right-overlap? arg1 arg2))
      false  [1 1]   [5 9]
      false  [10 10] [5 9]
      true   [9 10] [5 9]
      true   [1 10] [5 9]
      false  [1 9] [5 9]  ;; included !
      )))

(deftest range-test
  (testing "Creating ranges"
    (is (= [79 92]
           (d/create-range [79 14])))
    (is (= [79 79]
           (d/create-range [79 1])))))


(deftest intersaction-test
      (testing "get intersections between 2 ranges"
        (is (= [[5 15]]
               (d/intersection [1 10] [5 15]))  "left overlap")
        
        (is (= [[5 15]]
               (d/intersection [10 20] [5 15]))  "right overlap")
        )) 

(deftest apply-shift-rule-test
  (testing "applying rule on included"
    (is (= {:remain [], :mapped [81 94]}
           (d/apply-shift-rule  [79 92] [[50 97] 2])))
    (is (= {:remain [], :mapped [18 87]}
           (d/apply-shift-rule  [25 94]  [[25 94] -7]))))
  
  (testing "applying rule left overlap"
    (is (= {:remain [[5 14]], :mapped [17 22]}
           (d/apply-shift-rule  [5 20] [[15 25] 2])))
    
    (is (= {:remain [[5 19]], :mapped [22 22]}
           (d/apply-shift-rule  [5 20] [[20 25] 2])))
    
    #_(is (= {:remain [[5 19]], :mapped [22 22]}
           (d/apply-shift-rule  [5 30] [[20 25] 2])))
    )
  

  
  ) 