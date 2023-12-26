(ns day-8
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/8

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We have a network made of nodes. Each node has a name and two successors :
;; - a left 
;; - a right 

;; We start at node AAA and must navigate the network until we reach node ZZZ.
;; Navigation is done following instructions provided in the form of a string
;; made of L (for left) and R (for right) moves.

(def sample-input "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

;; First I thought to use zipper. Then I realized that the network can have loops so
;; it is not suitable to be represented as a zipper.
;; Maybe just a map will do

(def network-example {"AAA" ["BBB" "CCC"]
                      "BBB" ["DDD" "EEE"]})

(comment
  ;; The sequence of instructions is a seq of chars that repeat itself for ever.
  ;; To implement this, we can use cycle

  ;; For example : 
  (take 5 (cycle [\L \R]))
  (rest (take 5 (cycle [\L \R])))
  ;;


  ;; Now we can create a state data with this shape : 
  (def state-example {:network       network-example
                      :current-node ["AAA" ["BBB" "CCC"]]
                      :step-count   0})

  ;; Creating the initial state includes parsing input data
  (def initial (let [[moves _ & node-lines] (s/split-lines sample-input)]
                 {:network      (reduce (fn [acc line]
                                          (let [[[_ node-id left-id right-id]] (re-seq #"(\w\w\w) = \((\w\w\w), (\w\w\w)\)" line)]
                                            (assoc acc node-id (vector left-id right-id)))) {} node-lines)
                  :moves        (seq moves)
                  :current-node-id "AAA"
                  :step-count   0}))


  ;; Reduction of the initial state 

  (reduce (fn [state move]
            (tap> state)
            (if (= "ZZZ" (:current-node-id state))
              (reduced (:step-count state))
              (-> state
                  (update :step-count inc)
                  (update :current-node-id (fn [cur-node-id]
                                             (let [[left-id right-id] (get (:network state) cur-node-id)]
                                               (if (= \L move) left-id right-id)))))))
          initial
          '(\R \L \R \L \R \L \R \L \R \L)
          #_(take 10 (cycle (:moves initial))))

  ;; Let's create nice functions
  )

(defn create-network [node-lines]
  (reduce (fn [acc line]
            (let [[[_ node-id left-id right-id]] (re-seq #"(\w\w\w) = \((\w\w\w), (\w\w\w)\)" line)]
              (assoc acc node-id (vector left-id right-id)))) {} node-lines))

(defn parse-input [input]
  (let [[moves _ & node-lines] (s/split-lines input)]
    {:network         (create-network node-lines)
     :moves           (seq moves)
     :current-node-id "AAA"
     :step-count      0}))

(defn create-navigator [network]
  (fn [move current-node-id]
    (let [[left-id right-id] (get network current-node-id)]
      (if (= \L move) left-id right-id))))

(defn solution-1 [input]
  (let [initial-state (parse-input input)
        navigate      (create-navigator (:network initial-state))]
    (reduce (fn [state move]
              (tap> state)
              (if (= "ZZZ" (:current-node-id state))
                (reduced (:step-count state))
                (-> state
                    (update :step-count       inc)
                    (update :current-node-id  (partial navigate move)))))
            initial-state
            (cycle (:moves initial-state)))))

(comment

  (solution-1 sample-input)
  ;; still ok with sample input

  ;; Trying with puzzle input : 
  (solution-1 (slurp "resources/day_8.txt"))
  ;; => 15517 ⭐ one more star !
  )
