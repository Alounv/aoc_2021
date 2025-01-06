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
  (Integer/parseInt s))

(defn parse-bit  [s]
  (Integer/parseInt s 2))

(defn debug [s]
  (let [value (cond
                (keyword? s) (name s)
                (sequential? s) (str/join ", " (vec (doall s)))
                :else (str s))
        output (str value)]
    (println \newline)
    (println output)
    output))

;; Grid/Matrix helpers
(defn create-grid
  "Create a 2D vector from string input where each char becomes an element"
  [input]
  (->> input
       (str/split-lines)
       (mapv vec)))

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
    (is (= 6 (manhattan-distance [0 0] [2 4]))))

  (testing "debug"
    (is (= "7" (debug 7)))
    (is (= "MyName" (debug :MyName)))
    (is (= "{:a 7, :b 8}" (debug {:a 7 :b 8})))
    (is (= "7, 4, 9, 5, 11, 17" (debug [7 4 9 5 11 17])))
    (is (= "1, 2, 3, 4, 5, 6" (debug (range 1 7))))))
