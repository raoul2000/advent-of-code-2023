(ns day-7
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/7

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; playing Camels cards

(def sample-input "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

;; Hands types :
;; 1. Five of a kind - AAAAA   (5)
;; 2. Four of a kind - AA8AA   (4,1)
;; 3. Full House kind - AA8BB  (3,2)
;; 4. Three of a kind - TTT98  (3,1,1)
;; 5. Two pair - 23432         (2,2,1)
;; 6. One pair - A23A4         (2,1,1,1)
;; 7. High card - 23456        (1,1,1,1,1)

(comment
  ;; we need several predicates to identify each hand type
  ;; By identifying cards distribution we can esasely find hands type
  ;; For example : 
  )

(defn hand-fingerprint [s]
  (sort > (map second (frequencies s))))

(comment
  (hand-fingerprint "AAAAA")
  ;; => (5) no other possibility than Five Of a Kind

  (hand-fingerprint "AA8BB")
  ;; => (2 2 1) - Counting items is not enough, we can then just compare 
  ;; with known fingerprint for Two Pair

  ;;
  )

(defn five-of-a-kind?  [fingerprint] (= 1 (count fingerprint)))
(defn four-of-a-kind?  [fingerprint] (= '(4 1) fingerprint))
(defn full-house?      [fingerprint] (= '(3 2) fingerprint))
(defn three-of-a-kind? [fingerprint] (= '(3 1 1) fingerprint))
(defn two-pair?        [fingerprint] (= '(2 2 1) fingerprint))
(defn one-pair?        [fingerprint] (= '(2 1 1 1) fingerprint))

(defn hand-type [^String hand]
  (let [fingerprint (hand-fingerprint hand)]
    (cond
      (five-of-a-kind?  fingerprint)   1
      (four-of-a-kind?  fingerprint)   2
      (full-house?      fingerprint)   3
      (three-of-a-kind? fingerprint)   4
      (two-pair?        fingerprint)   5
      (one-pair?        fingerprint)   6
      :else                            7)))

(comment
  (hand-type "AAAAA")
  (hand-type "AAAAB")
  (hand-type "AAABB")
  (hand-type "AAADF")
  (hand-type "AADFF")
  (hand-type "ZADFF")
  (hand-type "ZADFY")

  ;; Ok we are now able, given a card hand, to get its type. Now, we must also
  ;; be able to sort 2 or more hands of the same type, and for this, we must compare
  ;; each hand card in given order. So we need a way to compare two cards.
  ;; Cards are : A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2.

  (def card-label [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2])

  ;; By using a vector, we preserve order (via item index)
  ;;
  (sort-by identity (fn [a b]
                      ;; comparator
                      0) ["AK" "AT"])
  ;; let's createt a card hand comparator function based on 
  ;; cards values.
  ;; 
  (.indexOf card-label \A)
  (.indexOf card-label \K)
  (.indexOf card-label \2)
  (.indexOf card-label \X)

  ;;
  )

(def card-label [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2])


(defn create-cards-comparator
  "Creates and returns a card comparator fn, given an ordered list of
   cards, from higher to lower."
  [ordered-cards]
  (fn [c1 c2]
    (if (= c1 c2)
      0
      (let [v1 (.indexOf ordered-cards c1)
            v2 (.indexOf ordered-cards c2)]
        (if (< v1 v2) 1 -1)))))


(comment
  (def card-comparator (create-cards-comparator card-label))

  ;; Sort from smallest to highest hand
  (sort-by identity card-comparator ["8" "A" "BAK"])

  ;; So, we can now sort hands with the same type.

  ;; To compare 2 hands, skip all identical cards starting from the first one
  ;; until distincts cards are found. Then compare those 2 distinct cards
  (->> (map vector "12abc" "12def")
       (drop-while (fn [[left right]]
                     (= left right)))
       first)
  ;;
  )

(defn create-hands-by-card-comparator
  "Creates and returns a comparator fn for same type hands"
  [card-comparator]
  (fn [h1 h2]
    (if (= h1 h2)
      0
      (let [[card-1 card-2] (->> (map vector h1 h2)
                                 (drop-while (fn [[char-1 char-2]] (= char-1 char-2)))
                                 first)]
        (card-comparator card-1 card-2)))))

(comment
  ;; Now, given a list of cards hands we should : 
  ;; - parse them 
  ;; - assign to each hand its type
  ;; - group the by type and for each one, sort them by card value
  ;; - flattent the result into an array
  ;; - using each item's index, compute the final result

  ;; Parse first : 
  (->> (re-seq #"(\w+) (\d+)" sample-input)
       (map (fn [[_match hand bid]]
              (vector hand (Integer/parseInt bid)))))
  ;; => (["32T3K" 765] ["T55J5" 684] ["KK677" 28] ["KTJJT" 220] ["QQQJA" 483])

  (def step-1 '(["32T3K" 765] ["T55J5" 684] ["KK677" 28] ["KTJJT" 220] ["QQQJA" 483]))

  ;; assign hand type
  (map #(conj % (hand-type (first %))) step-1)
  ;; => (["32T3K" 765 6] ["T55J5" 684 4] ["KK677" 28 5] ["KTJJT" 220 5] ["QQQJA" 483 4])

  (def step-2 '(["32T3K" 765 6] ["T55J5" 684 4] ["KK677" 28 5] ["KTJJT" 220 5] ["QQQJA" 483 4]))

  (def card-comparator (create-cards-comparator card-label))
  (def hand-by-card-comparator (create-hands-by-card-comparator card-comparator))

  (reverse (sort-by first hand-by-card-comparator [["QQQJA" 483 4] ["T55J5" 684 4]]))

  (def hand-type last)
  (def cards first)

  (->> (group-by hand-type step-2)
       (map (fn [[type hands]]
              (tap> (count hands))
              (vector type (if (>  (count hands) 1)
                             (sort-by cards hand-by-card-comparator hands)
                             hands))))
       (sort-by first >) ;; decreasing hond type
       (reduce (fn [acc [_type hands]]
                 (into acc hands)) [])
       (reduce-kv (fn [acc k [_cards bid _type]]
                    (+ acc (* bid (inc k)))) 0))
  ;;=> 6440 👍 sample input give good result
  ;; Let's clean up and create nice functions
  ;;
  )

(defn parse-input
  "Given puzzle input, returns a seq of pairs where the first one is the card hand and
   the second the bid."
  [input]
  (->> (re-seq #"(\w+) (\d+)" input)
       (map (fn [[_match hand bid]]
              (vector hand (Integer/parseInt bid))))))

(comment
  (parse-input sample-input)
  ;;
  )

(def assign-hand-type #(conj % (hand-type (first %))))

(defn sort-hands-of-same-type [hand-by-card-comparator]
  (fn [[type hands]]
    (vector type (if (> (count hands) 1)
                   (sort-by first hand-by-card-comparator hands)
                   hands))))


(defn solution-1 [input]
  (let [card-comparator         (create-cards-comparator         card-label)
        hand-by-card-comparator (create-hands-by-card-comparator card-comparator)
        hand-type                last]
    (->> input
         parse-input
         (map assign-hand-type)
         (group-by hand-type)
         (map (sort-hands-of-same-type hand-by-card-comparator))
         ;; decreasing hand type order
         (sort-by first >) 
         ;; flatten and get rid of hand type key
         (reduce (fn [acc [_type hands]] 
                   (into acc hands)) [])
         ;; compute result
         (reduce-kv (fn [acc k [_cards bid _type]]
                      (+ acc (* bid (inc k)))) 0)))
  ;;
  )

(comment
  (solution-1 sample-input)
  ;; still ok with sample input

  ;; Trying with puzzle input : 
  (solution-1 (slurp "resources/day_7.txt"))
  ;; => 250453939 ⭐ yes yes yes !! first shot !
  )

