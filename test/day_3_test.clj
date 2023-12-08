(ns day-3-test
  (:require [clojure.test :refer [deftest testing are is]]
            [day-3 :as d]))

(deftest identity-chars-test
  (testing "recongnize symbols"
    (are [result c] (= result (d/is-symbol-char? c))
      true   \$
      true   \@
      true   \#
      true   \a
      false  \.
      false  \1
      false  \9
      false  \0))

  (testing "recongnize digit"
    (are [result c] (= result (d/is-digit-char? c))
      false   \$
      false   \@
      false   \#
      false   \a
      false  \.
      true   \1
      true   \9
      true   \0)))

(deftest grid-helpers-test
  (testing "compute grid dimensions"
    (is (= {:col-count 3 :row-count 2}
           (d/grid-dimensions [[1 2 3]
                               [1 2 3]])))
    (is (= {:col-count 0 :row-count 0}
           (d/grid-dimensions []))))
  (testing "coord in grid ?"
    (let [in-2x3? (d/in-grid? {:col-count 2, :row-count 3})]
      (is (true? (in-2x3? [1 1])))
      (is (true? (in-2x3? [0 0])))
      (is (true? (in-2x3? [1 2])))
      (is (false? (in-2x3? [2 1])))
      (is (false? (in-2x3? [1 3])))
      (is (false? (in-2x3? [2 3])))
      
      ))) 