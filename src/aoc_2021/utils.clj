(ns aoc-2021.utils
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; File handling
(defn read-input
  "Read input file for a given day number"
  [day]
  (-> (format "resources/%02d.txt" day)
      slurp))

;; Parsing helpers
(defn get-lines
  "Get lines from string input, removing empty lines"
  [input]
  (->> input
       str/split-lines
       (map str/trim)
       (filter #(not (str/blank? %)))))

(defn get-blocks
  "Get blocks from string input by splitting on \n"
  [input]
  (as-> input $
    (str/split $ #"\n\n")))

(defn parse-int [s]
  (let [c (str s)]
    (Integer/parseInt c)))

(defn parse-bit  [s]
  (Integer/parseInt s 2))

;; Grid/Matrix helpers
(defn grid-create
  "Create a 2D vector from string input where each char becomes an element"
  [input]
  (->> input
       (str/split-lines)
       (mapv vec)))

(defn grid-get
  "Get value from 2D grid at coordinates [x y]"
  [grid [x y]]
  (get-in grid [y x]))

(defn grid-list
  "Get a list of values from a grid"
  [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    (grid-get grid [x y])))

(defn create-empty-grid
  "Create a 2D vector of size [y x] filled with nil"
  [mx my]
  (vec (repeat my (vec (repeat mx nil)))))

;; Points helpers

(defn get_max_from_points
  "Get max dimensions from a list of points"
  [points]
  (reduce (fn [[mx my] [x y]]
            [(max mx x)
             (max my y)])
          points))

(defn grid-str
  "Get a string representation of a grid"
  [grid]
  (->> grid
       (map (fn [row] (map #(if (nil? %) \. %) row)))
       (map #(str/join "" %))
       (str/join "\n")))

;; Common operations
(defn manhattan-distance
  "Calculate Manhattan distance between two points"
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

;; Debug
(defn stringify [s]
  (let [value (cond
                (and (vector? s) (vector? (first s))) (grid-str s)
                (keyword? s) (name s)
                (sequential? s) (str/join ", " (vec (doall s)))
                :else (str s))
        output (str value)]
    output))

(defn debug [s]
  (println \newline)
  (println (stringify s)))

;; Tests for utilities
(deftest utils-test
  (testing "manhattan-distance"
    (is (= 6 (manhattan-distance [0 0] [2 4]))))

  (testing "create-grid"
    (is (= [[\1 \2 \3]
            [\4 \5 \6]
            [\7 \8 \9]]
           (grid-create "123\n456\n789"))))

  (testing "grid-get"
    (is (= \1 (grid-get [[\1 \2 \3]
                         [\4 \5 \6]
                         [\7 \8 \9]]
                        [0 0]))))

  (testing "grid-list"
    (is (= [\1 \2 \3 \4 \5 \6 \7 \8 \9]
           (grid-list [[\1 \2 \3]
                       [\4 \5 \6]
                       [\7 \8 \9]]))))

  (testing "create-empty-grid"
    (is (= [[nil nil nil]
            [nil nil nil]
            [nil nil nil]]
           (create-empty-grid 3 3))))

  (testing "get_str"
    (is (= "7" (stringify 7)))
    (is (= "MyName" (stringify :MyName)))
    (is (= "{:a 7, :b 8}" (stringify {:a 7 :b 8})))
    (is (= "7, 4, 9, 5, 11, 17" (stringify [7 4 9 5 11 17])))
    (is (= "1, 2, 3, 4, 5, 6" (stringify (range 1 7))))
    (is (= "...\n...\n..." (stringify (create-empty-grid 3 3))))
    (is (= "123\n456\n789" (stringify (grid-create "123\n456\n789"))))))
