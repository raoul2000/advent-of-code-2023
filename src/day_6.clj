(ns day-6
  (:require [clojure.string :as s]
            [clojure.math :as m]))

;; https://adventofcode.com/2023/day/6

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def sample-input "Time:      7  15   30
Distance:  9  40  200")

;; First we'll have to parse input to create a list of integers pairs [t d];
;; - 't' : is the race duration time
;; - 'd' : is the current distance record
;;
;; For a given d and t we must find all x where x*(t - x) > d  with  0 < x < t
;; We must then count how many x where found for each pair, and multiply them all
;; 
;; First hings first : parsing the input

(comment
  (->> (s/split-lines sample-input)
       (map #(->> (re-seq #"\d+" %)
                  (map (fn [n] (Integer/parseInt n)))))
       (apply map vector))
  ;;
  )

(defn parse-input [input]
  (->> (s/split-lines input)
       (map #(->> (re-seq #"\d+" %)
                  (map (fn [n] (Integer/parseInt n)))))
       (apply map vector)))

(comment
  (parse-input sample-input)
  (parse-input (slurp "resources/day_6.txt")))


;; Ok, now we have our pairs of integer, we need to process each one
(defn wins [[t d]]
  (filter (fn [x]
            (> (* x (- t x)) d)) (range 1 (dec d))))

(comment
  (count (wins [7 9]))
  (count (wins [15 40]))
  (count (wins [30 200]))

  ;; It seems we have enough to solve part 1
  )

(defn solution-1 [input]
  (->> input
       parse-input
       (map (comp count wins))
       (reduce *)))

(comment
  ;; test on sample input
  (solution-1  sample-input)
  ;; => 288  ok

  ;; ...adn now ...
  (solution-1 (slurp "resources/day_6.txt"))
  ;; => 1159152 good ! ⭐

  ;; Now, this part 1 was too simple compared to previous days ... 
  ;; The part 2 may be muuch more complex I guess. Let's see
  )
