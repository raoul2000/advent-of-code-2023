(ns day-10
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/10

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We'll have to navigate into a 2x2 grid just like on day 3, so we may reuse
;; function from there.

(def sample-input-1 "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
")

(def sample-input-2 "-L|F7
7S-7|
L|7||
-L-J|
L|-JF
")
;; At each position in the grid, there is a char. It can be a :
;; - 'S' : the start position hiding a pipe char
;; - a pipe char
;; - '.' : the ground.

;; Some Adjacent pipe chars can connect with others. We must represent this
;; data

(def pipe-connectors {\|  #{:north :south}
                      \-  #{:west  :east}
                      \L  #{:north :east}
                      \J  #{:north :west}
                      \7  #{:south :west}
                      \F  #{:south :west}
                      \S  #{:north :south :east :west}})

;; Given a position with a pipe in it. It can connect in a directions only 
;; if in this direction there is a pipe that can connect to the opposite direction
;; For example:
;; - | can connect :north and :south if
;;    - in the position at :north, there is a pipe that can connect :south
;;    - in the position at :south there is a pipe that can connect :north

(defn complement-direction [direction]
  (case direction
    :north   :south
    :south   :north
    :east    :west
    :west    :east
    (throw (Exception. (format "invalid direction : %s" direction)))))

(comment
  (complement-direction :north)
  (complement-direction :north-west)
  ;;
  )

(defn find-matching-pipes
  "Given a *pipe* char and a *direction* returns the set of pipes that
   could connect or nil if there is none.
   Note that \\S is never returned as it is not considered as a pipe part to connect to.
   It is only a starting point."
  [pipe direction]
  (let [connectors (get pipe-connectors pipe)]
    (when-let [connection-from  (connectors direction)]
      (let [connection-to (complement-direction connection-from)]
        (->> pipe-connectors
             (filter (fn [[candidate-pipe candidate-connectors]]
                       (when-not (= \S candidate-pipe)
                         (candidate-connectors connection-to))))
             (map first)
             (set))))))

(def matching-pipes (memoize find-matching-pipes))

(comment
  (find-matching-pipes \| :north)
  ;; => [\| \7 \F]
  (find-matching-pipes \| :west)
  ;; => nil
  (find-matching-pipes \- :west)
  ;; => (\- \L)
  (find-matching-pipes \- :east)
  ;; => (\- \J \7 \F)
  (find-matching-pipes \- :south)
  ;; => nil
  (matching-pipes \- :west)
  (matching-pipes \S :west)

  ;;
  )

;; Given the initial position S, that is connected to the loop, explore the complete
;; loop and returns steps count to reach thefarthest pipe.

(defn input->grid
  [s]
  (->> s
       (s/split-lines)
       (mapv vec)))


(defn grid-dimensions
  "Given a grid, returns a map describing grid dimensions in terms of col and row count"
  [grid]
  {:col-count (count (first grid))
   :row-count (count grid)})

(defn create-grid [s]
  (let [grid (input->grid s)]
    (merge {:matrix grid} (grid-dimensions grid))))


(comment
  (create-grid sample-input-1)
  (create-grid sample-input-2)
  ;;
  )

(defn in-grid [{:keys [col-count row-count]}]
  (fn [[x y]]
    (and (<= 0 x (dec col-count))
         (<= 0 y (dec row-count)))))

(defn char-at [[x y] grid]
  (-> (:matrix grid)
      (nth y)
      (nth x)))

;; Allowed moves are only : up down left right
(defn adjacent-coords [[x y] grid]
  (let [in-grid? (in-grid grid)]
    (->> (vector [(inc x)  y]
                 [(dec x)  y]
                 [x (inc y)]
                 [x (dec y)])
         (filter in-grid?))))

(defn adjacent-char-coll [[x y] grid]
  (map #(char-at % grid) (adjacent-coords [x y] grid)))