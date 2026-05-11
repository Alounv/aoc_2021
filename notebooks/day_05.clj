;; # 🌋 Day 5: Hydrothermal Venture
;; https://adventofcode.com/2021/day/5
(ns day-05
  (:require
   [aoc-2021.utils :refer [get-lines parse-int read-input]]
   [clojure.string :as str]))

;; ## Inputs
(def example
  "0,9 -> 5,9
  8,0 -> 0,8
  9,4 -> 3,4
  2,2 -> 2,1
  7,0 -> 7,4
  6,4 -> 2,0
  0,9 -> 2,9
  3,4 -> 1,4
  0,0 -> 8,8
  5,5 -> 8,2")

(def input (read-input 5))

;; ## Parsing
;; Each line `"x1,y1 -> x2,y2"` becomes a pair of endpoints `[[x1 y1] [x2 y2]]`.
(defn parse-line [line]
  (->> (str/split line #" -> ")
       (map #(str/split % #","))
       (map #(map parse-int %))
       (mapv vec)))

(defn parse [input]
  (->> input
       get-lines
       (map parse-line)))

(def parsed-example (parse example))
(def parsed-input (parse input))

;; ## Shared geometry
;; All integer points on the segment from `a` to `b`. Works for horizontal,
;; vertical and exact 45° diagonals.
;;
;; `(compare a b)` returns -1/0/1, exactly the per-axis step direction.
;; The number of points on the segment is the Chebyshev distance + 1.
(defn line-points [[x1 y1] [x2 y2]]
  (let [dx (compare x2 x1)
        dy (compare y2 y1)
        n  (inc (max (abs (- x2 x1)) (abs (- y2 y1))))]
    (take n (iterate (fn [[x y]] [(+ x dx) (+ y dy)]) [x1 y1]))))

;; A horizontal segment example — note both endpoints are included.
(line-points [0 9] [5 9])

;; A diagonal example — same stepping logic.
(line-points [0 0] [3 3])

(defn horizontal-or-vertical? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

;; Expand the given segments into every point they cover, count how often
;; each point is hit, then count points hit by at least two segments.
(defn count-overlaps [segments]
  (->> segments
       (mapcat #(apply line-points %))
       frequencies
       vals
       (filter #(> % 1))
       count))

;; ## Part 1
;; Consider only horizontal and vertical lines. Let's walk the example
;; through the pipeline step by step.

;; Keep only horizontal/vertical segments:
(def hv-example-segments
  (filter horizontal-or-vertical? parsed-example))

;; Expand every segment to its points:
(def hv-example-points
  (mapcat #(apply line-points %) hv-example-segments))

;; How many times each point is covered (showing only points hit twice or more):
(def hv-example-overlap-counts
  (->> hv-example-points
       frequencies
       (filter (fn [[_ n]] (> n 1)))
       (into {})))

;; The answer is just the number of such points:
(defn logic-1 [segments]
  (->> segments
       (filter horizontal-or-vertical?)
       count-overlaps))

(def answer-1-example (logic-1 parsed-example))
(def answer-1-input (logic-1 parsed-input))

;; ## Part 2
;; Same pipeline, but we drop the horizontal/vertical filter so the 45°
;; diagonals contribute too.
(defn logic-2 [segments]
  (count-overlaps segments))

(def answer-2-example (logic-2 parsed-example))
(def answer-2-input (logic-2 parsed-input))

;; ## Regression checks
(assert (= 5 answer-1-example))
(assert (= 5092 answer-1-input))
(assert (= 12 answer-2-example))
(assert (= 20484 answer-2-input))
