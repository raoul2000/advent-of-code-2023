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

