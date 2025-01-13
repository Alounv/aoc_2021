(ns aoc-2021.05
  (:require
   [aoc-2021.utils :refer [create-empty-grid get-lines get_max_from_points
                           grid-list parse-int read-input stringify]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

;; Parse

(defn parse-line [line]
  (->> (str/split line #" -> ")
       (map #(str/split % #","))
       (map #(map parse-int %))
       (mapv vec)))

(defn parse [input]
  (->> input
       get-lines
       (map parse-line)))

;; Logic

(defn get_max_from_pairs [pairs]
  (->> pairs
       (mapcat identity) ;; flatten
       (get_max_from_points)))

(defn get-diagonal
  "Get diagonal from a and b with all intermediate points"
  [[x1 y1] [x2 y2]]
  (let [x-step (if (< x1 x2) 1 -1)
        y-step (if (< y1 y2) 1 -1)
        x-seq (range x1 (+ x2 x-step) x-step)
        y-seq (range y1 (+ y2 y-step) y-step)]
    (map vector x-seq y-seq)))

(defn extrapolate
  "Extrapoplate all points between a and b"
  [has-diag [ax ay] [bx by]]
  (let [max-x (max ax bx)
        max-y (max ay by)
        min-x (min ax bx)
        min-y (min ay by)]

    (cond
      (= ax bx) (map (fn [y] [ax y]) (range min-y (inc max-y)))
      (= ay by) (map (fn [x] [x ay]) (range min-x (inc max-x)))
      :else (if has-diag (get-diagonal [ax ay] [bx by]) []))))

(defn update_point [curr] (if curr (inc curr) 1))

(defn get-grid
  "Get grid with the value being the number of points on each location"
  [has-diag pairs]
  (let [[mx my] (get_max_from_pairs pairs)
        empty-grid  (create-empty-grid (inc mx) (inc my))
        points (->> pairs
                    (map (fn [[a b]] (extrapolate has-diag a b)))
                    (mapcat identity))]

    (reduce  (fn [grid [x y]]
               (update-in grid [y x] update_point))
             empty-grid points)))

(defn count-gt-1
  "Count the number of points greated than 1"
  [grid]
  (->> grid
       grid-list
       (filter some?)
       (map parse-int)
       (filter #(> % 1))
       count))

(defn logic [has-diag input]
  (->> input
       parse
       (get-grid has-diag)
       count-gt-1))

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

  (testing "Get max"
    (is (= [9 9] (get_max_from_pairs (parse example)))))

  (testing "Get grid"
    (is (= ".......1..
..1....1..
..1....1..
.......1..
.112111211
..........
..........
..........
..........
222111...." (stringify (get-grid false (parse example))))))

  (testing "Count gt 1"
    (is (= 2 (count-gt-1 [[\1 \2 \3]]))))

  (testing "Example"
    (is (= 5 (logic false example))))

  (testing "Input"
    (is (= 5092 (logic false input)))))

;; Part 2

(deftest part2
  (testing "Get grid"
    (is (= "1.1....11.
.111...2..
..2.1.111.
...1.2.2..
.112313211
...1.2....
..1...1...
.1.....1..
1.......1.
222111...."
           (stringify (get-grid true (parse example))))))

  (testing "Example"
    (is (= 12 (logic true example))))

  (testing "Input"
    (is (= 20484 (logic true input)))))
