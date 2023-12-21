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

  (defn hand-fingerprint [s]
    (sort > (map second (frequencies s))))

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
  ;; cards are : A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2.

  (def card-label [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2])

  ;; By using a vector, we preserve order (via item index)
  ;;
  (sort-by identity (fn [a b ] 
                      ;; comparator
                      0) ["AK" "AT"])
  ;; let's create to implement a card hand comparator function based on 
  ;; cards values
  )
