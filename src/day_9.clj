(ns day-9
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/9

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each line in the input are sensor history values

(def sample-input "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
")

;; Part 1 consist in :
;; - reducing each history line into a list of values until all values are zero
;; - computing from last to first list, the next value in the history seq

;; First : parsing
(defn parse-sensor-history [input]
  (->> (s/split-lines input)
       (mapv (fn [line]
               (mapv #(Integer/parseInt %) (s/split line #" "))))))

(comment
  (parse-sensor-history sample-input)
  ;;
  )

;; detect a stop condition : seq of zero
(defn done? [xs]
  (every? zero? xs))

(comment
  (done? [1 2 3])
  (done? [1 0 0])
  (done? [0 0 0])
  ;;
  )

;; Better check the opposite condition
(defn not-done? [xs]
  (some pos-int? xs))

(comment
  (not-done? [0 0 0 1])
  (not-done? [0 0 0 0])
  ;;
  )

(comment
  ;; at last, derivate the history values
  (defn derive-history [xs]
    (mapv (fn [[a b]] (- b a)) (partition 2 1 xs)))

  (def result (take-while not-done? (iterate derive-history [10  13  16  21  30  45])))

  ;; Eventually reduce the previous result
  (reduce (fn [acc xs]
            (+ acc (last xs))) 0 (reverse result))

  ;; Let's create function now 
  )

(defn derive-history [xs]
  (mapv (fn [[a b]] (- b a)) (partition 2 1 xs)))

(defn prediction [sensor-history]
  (let [derived-values (take-while not-done? (iterate derive-history sensor-history))]
    (reduce (fn [acc xs]
              (+ acc (last xs))) 0 (reverse derived-values))))

(comment
  (prediction [10  13  16  21  30  45])
  ;;
  )

(defn solution-1 [input]
  (->> input
       parse-sensor-history
       (map prediction)
       (reduce +)))

(comment
  (solution-1 sample-input)
  ;; => 114 This is the expected result with sample input.

  ;; let's try now with puzzle inputs
  (solution-1 (slurp "resources/day_9.txt"))
  ;; => 1708216566 😠 .. what ? "Too high" ? I was sure I was going to get a new star
  ;;

  ;; Maybe there's some error related to negatives values ? 
  ;; let's decompose 
  ;; 
  '([4 7 14 17 8 -3 40]
    [3 7 3 -9 -11 43]
    [4 -4 -12 -2 54]
    [-8 -8 10 56]
    [0 18 46]
    [18 28]
    [10])

  (prediction [4 7 14 17 8 -3 40])
  (take-while not-done? (iterate derive-history [4 7 14 17 8 -3 40]))
  
  
  ;;
  )




