(ns day-5
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/5

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sample-input "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

;; We have to send a seq of number into several mapping and choose among all the output
;; the smallest one.
;; Each mapping follows the same algo.
;; 
;; A map is made of lines, each one made of 3 numbers
;; - destination range start
;; - source range start
;; - range length
;; Note that destination and source ranges have the same length
;;
;; If the number to map is in the source range start, return its corresponding value in the destination
;; range, otherwise return the input number unchanged
;; 
;; Before parsing the input data, let's make some experiments 

(comment

  ;; This hard coded snippet does the trick
  (let [[dest-start source-start range-len]  [52 50 48]
        input-val 13]
    (if (> (+ source-start range-len) input-val (dec source-start))
      (+ dest-start (- input-val source-start))
      input-val))

  ;; Let's extract a function 
  (defn in-source-range [v [_dest-start source-start range-len]]
    (when (> (+ source-start range-len) v (dec source-start))
      [_dest-start source-start range-len]))

  (in-source-range 53 [52 50 48])
  (in-source-range 31 [52 50 48])

  (defn map-to-dest [v [dest-start source-start _range-len]]
    (+ dest-start (- v source-start)))

  (map-to-dest 53 [52 50 48])

  ;; Now if a map is represented as a seq of [dest-start source-start range-len] 
  ;; we could apply mapping to input value v like this:

  (defn source->dest [v map-ranges]
    (if-let [winning-range (some #(in-source-range v %) map-ranges)]
      (map-to-dest v winning-range)
      v))

  ;; test again with sample input :

  (source->dest 79 [[50 98 2]
                    [52 50 48]])

  (source->dest 14 [[50 98 2]
                    [52 50 48]])

  (source->dest 55 [[50 98 2]
                    [52 50 48]])

  (source->dest 13 [[50 98 2]
                    [52 50 48]])
  ;; 👍 

  ;; It is now time to think about parsing the input string to create a 
  ;; seq of maps
  ;; First extraxct the seeds: Applying following on the first line only
  (map #(Integer/parseInt %) (re-seq #"\d+" (first (s/split-lines sample-input))))

  ;; now starting from line 2, extract all maps
  (map #(re-seq #"\d+" %) (rest (s/split-lines sample-input)))

  (->> sample-input
       s/split-lines
       rest
       (map #(re-seq #"\d+" %))
       (map #(if (nil? %)
               nil
               (map (fn [s]
                      (Integer/parseInt s)) %)))
       (partition-by nil?)
       (filter first))
  ;; It seems we have everything to solve part 1
  ;; Let's clean up and create nice functions for this
  )

(defn in-source-range [v [_dest-start source-start range-len]]
  (when (> (+ source-start range-len) v (dec source-start))
    [_dest-start source-start range-len]))

(defn map-to-dest [v [dest-start source-start _range-len]]
  (+ dest-start (- v source-start)))

(defn source->dest [map-ranges v]
  (if-let [winning-range (some #(in-source-range v %) map-ranges)]
    (map-to-dest v winning-range)
    v))

(defn create-maps [input]
  (->> input
       s/split-lines
       rest
       (map #(re-seq #"\d+" %))
       (map #(if (nil? %)
               nil
               (map (fn [s]
                      (Integer/parseInt s)) %)))
       (partition-by nil?)
       (filter first)))

(defn create-seeds [input]
  (map #(Integer/parseInt %) (re-seq #"\d+" (first (s/split-lines input)))))

(defn solution-1 [input]
  (let [seeds (create-seeds input)
        maps  (create-maps input)]
    (->> maps
         (reduce (fn [acc a-map]
                   (map #(source->dest a-map %) acc)) seeds)
         (reduce min))))

(comment
  ;; test on sample input
  (solution-1  sample-input)
  ;; => 35 excellent ! this is the expected result 

  ;; ...adn now ...
  (solution-1 (slurp "resources/day_5.txt"))
  ;; 
  ;; ; Error printing return value (NumberFormatException) at java.lang.NumberFormatException/forInputString (NumberFormatException.java:65).
  ;; For input string: "3640772818"
  ;;
  ;; 💥 boum !! 

  ;; This is probably because this throws: 
  (Integer/parseInt "3640772818") 

  ;; The puzzle input data contains number much more big than the sample inputs

  ;; We should have used : 
  (biginteger "3640772818")
  ;;
  )

(defn create-maps-1 [input]
  (->> input
       s/split-lines
       rest
       (map #(re-seq #"\d+" %))
       (map #(if (nil? %)
               nil
               (map (fn [s]
                      (biginteger s)) %)))
       (partition-by nil?)
       (filter first)))

(defn create-seeds-1 [input]
  (map #(biginteger %) (re-seq #"\d+" (first (s/split-lines input)))))

(defn solution-1-b [input]
  (let [seeds (create-seeds-1 input)
        maps  (create-maps-1 input)]
    (->> maps
         (reduce (fn [acc a-map]
                   (map #(source->dest a-map %) acc)) seeds)
         (reduce min))))

(comment
  ;; test on sample input
  (solution-1-b  sample-input)
  ;; => 35N still ok

  ;; ...adn now ...
  (solution-1-b (slurp "resources/day_5.txt"))
  ;; => 214922730N Huraa ! another ⭐
  )





