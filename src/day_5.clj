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


;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; We must now consider the Seeds line, not as a list of seeds, but as a list of pairs describing seeds range
;; The first number is the start of the range, the second the length of the range.

;; 🛑 looking at puzzle inputs values I suspect an issue !
;; first pair is 3640772818 104094365 😮
;; This range contains a lot of seeds !! We may need to do some optimisation here otherwise we'll
;; wait for days before getting a result.

;; Let's ignore this potential issue for now.
;; So, this time we will create a list of sseds following new rules.


(comment
  ;; First range of the sample input :
  (range 79N (+ 79N 14N))

  (partition 2 [1 2 5 6])


  (defn create-seeds-2 [input]
    (->> (map #(biginteger %) (re-seq #"\d+" (first (s/split-lines input))))
         (partition 2)
         (mapcat (fn [[start len]]
                (range  start (+ start len))
                ))))
  
  (create-seeds-2 "seeds: 79 14 55 13")
  ;; with sample input : 
  (def puzzle-input "seeds: 3640772818 104094365 1236480411 161072229 376099792 370219099 1590268366 273715765 3224333694 68979978 2070154278 189826014 3855332650 230434913 3033760782 82305885 837883389 177854788 2442602612 571881366")
  
  (create-seeds-2 puzzle-input) ;; 💥 boom ... evaluating this form just hang the computer : too many values
  ;; first range contains 104 094 365 seeds .. more than 104 millons !!

  ;; couting all seeds in the range below takes 8 seconds
  (time (count (create-seeds-2 "seeds: 3640772818 104094365")))

  ;; Let's see what happen when we map this huge amount of seeds
  )

(defn create-seeds-2 [input]
  (->> (map #(biginteger %) (re-seq #"\d+" (first (s/split-lines input))))
       (partition 2)
       (mapcat (fn [[start len]]
                 (range  start (+ start len))))))

(defn solution-2 [input]
  (let [seeds (create-seeds-2 input)
        maps  (create-maps-1 input)]
    (->> maps
         (reduce (fn [acc a-map]
                   (map #(source->dest a-map %) acc)) seeds)
         (reduce min))))

(comment

  ;; First check if the result is correct with sample inputs
  (solution-2 sample-input)
  ;; yes, it is 46 as expected

  ;; Now let's play with a reduced set of seeds and the puzzle input data

  #_(solution-2 (slurp "resources/day_5_reduced.txt"))

  ;; Foooorget it !! 😭 .... take more than 10 minutes on my PC so there is no way
  ;; we'll get a solution, with this kind of algorithm from the part 1.

  ;; We need to think about another way ...

  ;; Why not try to see if we can work not with values, but with ranges ? 

  ;; Going back to sample input: 79 14
  ;; 
  ;; - seeds range [79 92] with  92 = (+ 79 (dec 14))) - (+ source-start (dec source-end))
  ;;
  ;; map 1 ------------------------------------------- seed-to-soil
  ;;  [98 99] => [50 52] - dest = source - 48
  ;;  [50 97] => [52 99] - dest = source + 2
  ;; => IN : proecess Range [79 92] :
  ;; - does not overlap with [98 99] : (no change)
  ;; - is included in [50 97] : the input interval should be translated +2
  ;; <== OUT [81 94]

  ;; map 2 ------------------------------------------ soil-to-fertilizer
  ;; [15 51] => [0 ... don't care] - dest = source -15   (15 = dest-start - source-start)
  ;; [52 53] => [37 .. don't care] - dest = source -15 
  ;; => IN : [81 94]
  ;; - [15 51] - no overlap
  ;; - [52 53] - no overlap
  ;; <== OUT [81 94]

  ;; map 3 ------------------------------------------- fertilizer-to-water
  ;; [53 60] => [49 ...] - dest = source - 4
  ;; [11 52] => [0  ...] - dest = source - 11
  ;; [0   6] => [42 ...] - dest = source + 42
  ;; [7  10] => [57 ...] - dest = source + 50
  ;; ==> IN [81 94]
  ;; no start range match
  ;; <== OUT [81 94]

  ;; map 4 -------------------------------------------- water-to-light
  ;; [18 24] => [88 ...] - dest = source + 70
  ;; [25 94] => [18 ...] - dest = source - 7
  ;; ==> IN [81 94]
  ;; - first does not match
  ;; - match ! 
  ;; <== OUT [74 87]

  ;; map 5 -------------------------------------------- light-to-temperature
  ;; [77 99] => [45 ...] => dest = source - 32
  ;; [45 63] => [81 ...] => dest = source + 36
  ;; [64 76] => [68 ...] => dest = source + 4
  ;; ==> INT [74 87]
  ;; - partial match 
  ;;      [74 76] no modified
  ;;      [77 87] -32 => [45 55] - mapping done, added to OUT
  ;; - range 2 : [74 76] no match
  ;; - range 3 : [74 76] is included in [64 76]
  ;;      [74 76] +4 => [78 80] added to OUT
  ;; <== OUT ( )




  ;;
  )




