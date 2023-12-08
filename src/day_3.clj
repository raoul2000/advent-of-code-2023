(ns day-3
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/3

;; we must explore an 'engine-schematic' which is provided as grid
;; in this grid we have numbers, characters '.' and symbols characters 
;; which are not digits nor character '.'

(defn is-symbol-char? [c]
  (not (or (Character/isDigit c)
           (= \. c))))

(defn is-digit-char? [c]
  (Character/isDigit c))

;; we must select in this grid, only number which are adjacent to a symbol 
;; and forget about the others. Then sum them up all together

;; I don't have any fancy idea to solve this pne (sorry)
;; The first idea is to browse the grid line by lines, and each time we 
;; find a digit, accumulate following digits on the same line : we have a number
; Then for each number find all the adjacent positions and search for a symbol 
;; in each one.
;; this means we need a way to navigate a 2 dimensions grid.
;;
;; let's try than

;; First we need to represent the grid

(def sample-input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

;; that's the easy part 😏

(defn input->grid
  [s]
  (->> s
       (s/split-lines)
       (mapv seq)))

(comment
  (input->grid sample-input)
  ;;
  )

(defn grid-dimensions
  "Given a grid, returns a map describing grid dimensions in terms of col and row count"
  [grid]
  {:col-count (count (first grid))
   :row-count (count grid)})

(comment
  (grid-dimensions (input->grid sample-input))
  ;;
  )

;; we will surely need grid dimensions in several places 
;; so why not create a map that contains the grid itself and its dimensions

(defn create-grid [s]
  (let [grid (input->grid s)]
    (merge {:grid grid} (grid-dimensions grid))))

(comment
  (create-grid)
  ;;
  )

;; now given a x,y coord, check if it matches a point inside the grid
;; we will choose 0,0 as the top left coords

(defn in-grid? [grid-dims]
  (fn [[x y]]
    (and (<= 0 x (dec (:col-count grid-dims)))
         (<= 0 y (dec (:row-count grid-dims))))))

(comment
  (def f (in-grid? {:col-count 2, :row-count 3}))
  (f [1 1])
  ;;
  )

;; get a seq of all coords when browsing the grid line by lines from top left
;; to bottom right

(defn coords-line-by-line [grid]
  (let [width  (grid-dimensions grid)
        height (grid-height grid)]
    (for [x (range 1 (dec width))
          y (range 1 (dec height))]
      [x y])))

;; get the char at the given coord





