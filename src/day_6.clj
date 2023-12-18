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

;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We must now consider only one pair, so the parsing operation
;; must be modified to concat each number from line 1 and each numbers from line 2

(comment
  (->> (s/split-lines sample-input)
       (map #(re-seq #"\d+" %))
       (map #(apply str %))
       (map #(Integer/parseInt %)))

  (count (wins [71530 940200]))
  ;; => 71503 This is the expected result for sample input
  ;; By looking at puzzle input we will probably need biginteger 

  ;; Let's try
  ;;
  )

(defn parse-input-2 [input]
  (->> (s/split-lines input)
       (map #(re-seq #"\d+" %))
       (map #(apply str %))
       (map #(biginteger %))))

(comment
  (parse-input-2 sample-input)
  ;;
  )

(defn solution-2 [input]
  (->> input
       parse-input-2
       wins
       count))

(comment
  ;; test on sample input
  (solution-2  sample-input)
  ;; => 71503 ... still good  ok

  ;; ...and now ...
  (solution-2 (slurp "resources/day_6.txt"))
  ;; Yeah, right 🛑 .. too big number, too much computing. Maybe some sort of 
  ;; quantic computer could do it, but my poor PC can't. We must think of a more clever
  ;; way to get the answer

  ;; Basically the pair for our puzzle input is : 
  (parse-input-2 (slurp "resources/day_6.txt"))
  ;; => (58819676 434104122191218) 😮 that's a lot


  ;; Let's try doing some math :
  
  ;; x*(t - x) > d  with  0 < x < t
  ;; - x^2 + t*x -d > 0
  ;; a = -1,  b = t, c = -d
  ;; Delta = b² - 4ac 
  ;;       = t² - 4(-1)(-d)
  ;;       = t² - 4d
  (count (wins [30 200]))
  ;; t = 30  d = 200
  (- (* 30 30) (* 4 200)) ;; => 100
  
  ;; x1 = (-b - sqr(delta)) / (2a)
  (/ (- (* -1 30) 10 ) -2) ;; => 20
  ;; x2 = (-b + sqr(delta)) / (2a)
  (/ (+ (* -1 30) 10) -2) ;; => 10
  ;; count integers between 10 and 20 => 9
  (- (dec 20) 10)

  ;; same with puzzle inputs
  ;; t = 58819676 d = 434104122191218)
  (def t 58819676N)
  (def d 434104122191218N)
  (def a -1)
  (def b t)
  (def c (* -1 d))
  (def delta (- (* b b) (* 4 (* a c))))

  (def x1 (- (* -1 b) (Math/sqrt delta)))
  (def x2 (+ (* -1 b) (Math/sqrt delta)))

  (biginteger (- (dec x2) x1))
  
  
  
  ;;
  )