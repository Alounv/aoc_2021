(ns aoc-2021.tmp
  (:require
   [aoc-2021.utils :refer [read-input get-lines]]
   [clojure.test :refer [deftest is testing]]))

;; Parse

(defn parse [input]
  (->> input
       get-lines))

;; Logic

(defn logic_1 [input]
  (->> input
       parse
       count))

;; Inputs

(def input (read-input 0))

(def example
  "
")

;; Tests

(deftest part1
  (testing "Example"
    (is (= 0 (logic_1 example))))

  (testing "Input"
    (is (= 0 (logic_1 input)))))
