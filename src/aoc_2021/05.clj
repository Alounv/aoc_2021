(ns aoc-2021.05
  (:require
   [aoc-2021.utils :refer [get-lines parse-int read-input]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

;; Parse

(defn parse-line [line]
  ;; "0,9 -> 5,9" => [[0 9] [5 9]]
  (->> (str/split line #" -> ")
       (map #(str/split % #","))
       (map #(map parse-int %))
       (mapv vec)))

(defn parse [input]
  (->> input
       get-lines
       (map parse-line)))

;; Logic

(defn line-points
  "All points on the line from a to b (horizontal, vertical, or 45° diagonal)."
  [[x1 y1] [x2 y2]]
  ;; compare on numbers returns -1/0/1 — exactly the per-axis step direction.
  ;;   (compare 5 3) => 1, (compare 3 5) => -1, (compare 3 3) => 0
  ;; n = Chebyshev distance + 1 (point count, both endpoints included).
  ;; e.g. (line-points [0 0] [3 3]) => ([0 0] [1 1] [2 2] [3 3])
  (let [dx (compare x2 x1)
        dy (compare y2 y1)
        n  (inc (max (abs (- x2 x1)) (abs (- y2 y1))))]
    (take n (iterate (fn [[x y]] [(+ x dx) (+ y dy)]) [x1 y1]))))

(defn logic [has-diag input]
  ;; expand each line into its points, then count points covered by ≥2 lines
  (->> input
       parse                                       ;; ([[0 9] [5 9]] ...)     — pairs of endpoints
       (filter (fn [[[x1 y1] [x2 y2]]]             ;; drop diagonals when has-diag is false
                 (or has-diag (= x1 x2) (= y1 y2))))
       (mapcat #(apply line-points %))             ;; ([0 9] [1 9] [2 9] ...) — flat list of every point
       frequencies                                 ;; {[0 9] 2, [1 9] 1, ...} — point -> hit count
       vals                                        ;; (2 1 1 ...)             — just the counts
       (filter #(> % 1))                           ;; (2 3 ...)               — only overlaps
       count))                                     ;; -> integer answer

;; Inputs

(def input (read-input 5))

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

;; Tests

(deftest part1
  (testing "Parse"
    (is (= [[[0 9] [5 9]]] (parse "0,9 -> 5,9")))
    (is (= [[[0 9] [5 9]]
            [[8 0] [0 8]]
            [[9 4] [3 4]]
            [[2 2] [2 1]]
            [[7 0] [7 4]]
            [[6 4] [2 0]]
            [[0 9] [2 9]]
            [[3 4] [1 4]]
            [[0 0] [8 8]]
            [[5 5] [8 2]]] (parse example))))

  (testing "Example"
    (is (= 5 (logic false example))))

  (testing "Input"
    (is (= 5092 (logic false input)))))

;; Part 2

(deftest part2
  (testing "Example"
    (is (= 12 (logic true example))))

  (testing "Input"
    (is (= 20484 (logic true input)))))
