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

(defn parse-int [s]
  (Integer/parseInt s))

;; Grid/Matrix helpers
(defn create-grid
  "Create a 2D vector from string input where each char becomes an element"
  [input]
  (mapv vec (str/split-lines input)))

(defn grid-get
  "Get value from 2D grid at coordinates [x y]"
  [grid [x y]]
  (get-in grid [y x]))

;; Common operations
(defn manhattan-distance
  "Calculate Manhattan distance between two points"
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

;; Tests for utilities
(deftest utils-test
  (testing "manhattan-distance"
    (is (= 6 (manhattan-distance [0 0] [2 4])))))
