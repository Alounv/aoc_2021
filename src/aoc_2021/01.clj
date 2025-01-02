(ns aoc-2021.01
  (:require
   [aoc-2021.utils :refer [read-input parse-int get-lines]]
   [clojure.test :refer [deftest is testing]]))

;; Parse

(defn parse [input]
  (->> input
       get-lines
       (map #(parse-int %)))) ;; Parse each line to integer

;; Logic

(defn count-increases [numbers]
  (->> numbers
       (partition 2 1) ;; Overlaping windows of size 2
       (filter (fn [[a b]] (> b a)))
       count))

(defn logic_1 [input]
  (->> input
       parse
       count-increases))

(defn sliding-window-increases [numbers]
  (->> numbers
       (partition 3 1) ;; Overlaping windows of size 3
       (map #(apply + %)) ;; Sum each window (apply "+" to each element)
       count-increases))

(defn logic_2 [input]
  (->> input
       parse
       sliding-window-increases))

;; Inputs

(def input (read-input 1))

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

;; Tests

(deftest part1
  (testing "Example"
    (is (= 7 (logic_1 example))))

  (testing "Input"
    (is (= 1676 (logic_1 input)))))

(deftest part2
  (testing "Example"
    (is (= 5 (logic_2 example))))

  (testing "Input"
    (is (= 1706 (logic_2 input)))))
