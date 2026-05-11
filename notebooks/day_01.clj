;; # 🌊 Day 1: Sonar Sweep
;; https://adventofcode.com/2021/day/1
(ns day-01
  (:require
   [aoc-2021.utils :refer [read-input parse-int get-lines]]))

;; ## Inputs
(def example
  "199
200
208
210
200
207
240
269
260
263")

(def input (read-input 1))

;; ## Parsing
;; Each line is a single integer depth reading.
(defn parse [input]
  (->> input
       get-lines
       (map parse-int)))

(def parsed-example (parse example))
(def parsed-input (parse input))

;; ## Part 1
;; Count the number of times a depth measurement increases from the previous
;; measurement.
(defn count-increases [numbers]
  (->> numbers
       (partition 2 1)
       (filter (fn [[a b]] (> b a)))
       count))

(def answer-1-example (count-increases parsed-example))
(def answer-1-input (count-increases parsed-input))

;; ## Part 2
;; Count the number of times the sum of a three-measurement sliding window
;; increases.
(defn sliding-window-increases [numbers]
  (->> numbers
       (partition 3 1)
       (map #(apply + %))
       count-increases))

(def answer-2-example (sliding-window-increases parsed-example))
(def answer-2-input (sliding-window-increases parsed-input))

;; ## Regression checks
(assert (= 7 answer-1-example))
(assert (= 1676 answer-1-input))
(assert (= 5 answer-2-example))
(assert (= 1706 answer-2-input))
