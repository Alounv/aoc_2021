;; # 🚤 Day 2: Dive!
;; https://adventofcode.com/2021/day/2
(ns day-02
  (:require
   [aoc-2021.utils :refer [get-lines read-input]]
   [clojure.string :as str]))

;; ## Inputs
(def example
  "forward 5
  down 5
  forward 8
  up 3
  down 8
  forward 2")

(def input (read-input 2))

;; ## Parsing
;; Each line is a direction (`forward`/`down`/`up`) and a magnitude. We turn
;; it into a `Move` record carrying horizontal and vertical deltas.
(defrecord Move [dx dy])

(defn parse-line [line]
  (let [[dir d] (str/split line #" ")
        d (Integer/parseInt d)
        dx (case dir "forward" d "backward" (- d) 0)
        dy (case dir "down" d "up" (- d) 0)]
    (->Move dx dy)))

(defn parse [input]
  (->> input
       get-lines
       (map parse-line)))

(def parsed-example (parse example))
(def parsed-input (parse input))

;; ## Part 1
;; Sum all horizontal deltas, sum all vertical deltas, multiply them.
(defn logic-1 [moves]
  (let [total-dx (reduce + (map :dx moves))
        total-dy (reduce + (map :dy moves))]
    (* total-dx total-dy)))

(def answer-1-example (logic-1 parsed-example))
(def answer-1-input (logic-1 parsed-input))

;; ## Part 2
;; `up`/`down` now adjust an `aim`. `forward d` moves horizontal by `d` and
;; depth by `d * aim`. Final answer is `x * y`.
(defrecord P [x y])

(defn logic-2 [moves]
  (loop [aim 0 position (->P 0 0) moves moves]
    (let [x (:x position) y (:y position)]
      (if (empty? moves)
        (* x y)
        (let [m (first moves)
              d_x (:dx m)
              d_aim (:dy m)
              new-aim (+ aim d_aim)
              new-pos (->P (+ x d_x)
                           (+ y (* d_x aim)))]
          (recur new-aim new-pos (rest moves)))))))

(def answer-2-example (logic-2 parsed-example))
(def answer-2-input (logic-2 parsed-input))

;; ## Regression checks
(assert (= 150 answer-1-example))
(assert (= 1893605 answer-1-input))
(assert (= 900 answer-2-example))
(assert (= 2120734350 answer-2-input))
