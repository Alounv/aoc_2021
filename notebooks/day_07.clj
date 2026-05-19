;; # Day 7
;; https://adventofcode.com/2021/day/7
(ns day-07
  (:require
   [aoc-2021.utils :refer [parse-int read-input]]
   [clojure.string :as str]))

;; ## Inputs
(def example "16,1,2,0,4,2,7,1,2,14")

(def input (read-input 7))

;; ## Parsing
(defn parse [input]
  (->> (str/trim input)
       (#(str/split % #","))
       (map parse-int)))

(def crabs-example (parse example))
(def crabs (parse input))

;; ## Part 1

(defn median [crabs]
  (let [sorted (sort-by identity crabs)
        middle (quot (count sorted) 2)]
    (nth sorted middle)))

(def median-example (median crabs-example))
(def median-input (median crabs))

(defn fuel-1 [positions target]
  (->> positions
       (map #(Math/abs (- % target)))
       (reduce +)))

(def answer-1-example (fuel-1 crabs-example median-example))
(def answer-1 (fuel-1 crabs  median-input))

;; ## Part 2

;; 1 -> 1 / 2 -> 3 / 3 -> 6
(defn calc [x]
  (->> (inc x)
       (range 1)
       (reduce +)))

(defn fuel-2 [positions target]
  (->> positions
       (map #(Math/abs (- % target)))
       (map calc)
       (reduce +)))

(defn mean [crabs]
  (let [count (count crabs)
        sum (reduce + crabs)]
    (quot sum count)))

(def mean-example (mean crabs-example))
(def mean-input (mean crabs))

(fuel-2 crabs-example (+ mean-example 1))
(fuel-2 crabs-example mean-example)
(fuel-2 crabs-example (- mean-example 1))

(def answer-2-example (fuel-2 crabs-example (+ mean-example 1)))

(fuel-2 crabs (+ mean-input 1))
(fuel-2 crabs mean-input)
(fuel-2 crabs (- mean-input 1))

(def answer-2 (fuel-2 crabs mean-input))

;; ## Regression checks

(assert (= answer-1-example 37))
(assert (= answer-1 352254))

(assert (= answer-2-example 168))
(assert (= answer-2 99053143))
