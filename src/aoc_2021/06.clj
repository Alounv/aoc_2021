(ns aoc-2021.06
  (:require
   [aoc-2021.utils :refer [get-lines parse-int read-input]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

;; Parse
(defn parse-line [line]
  (->> (str/split line #",")
       (map  parse-int)))

(defn categorize [numbers]
  (reduce (fn [acc n]
            (update acc n (fnil inc 0))) ;; Increment position "n" in accumulator
          [0 0 0 0 0 0 0 0 0] ;; Initial count for number 0
          numbers))

(defn parse [input]
  (->> input
       get-lines
       first
       parse-line
       categorize))

;; Logic

(defn update-one-day [categories]
  (let [fst (first categories) ;; number of 0 days fishes
        rst (rest categories)] ;; rest
    (-> (conj (vec rst) fst) ;; put the number of 0 days fishes in the end (8 days)
        (update 6 #(+ (or % 0) fst)))))

(defn update-n-days [n categories]
  (reduce (fn [acc _]
            (update-one-day acc))
          categories
          (range n)))

(defn logic [input n]
  (->> input
       parse
       (update-n-days n)
       (reduce +)))

;; Inputs

(def input (read-input 6))

(def example "3,4,3,1,2")
;; Tests

(deftest parse-test
  (testing "Parse function"
    (is (= [0 1 1 2 1 0 0 0 0] (parse example)))))

(deftest update-one-day-test
  (testing "Update one day"
    (is (= [1 1 2 1 0 0 0 0 0] (update-one-day (parse example))))))

(deftest update-n-days-test
  (testing "Update n days"
    (is (= [1 2 1 0 0 0 1 0 1] (update-n-days 2 (parse example))))))

(deftest part1
  (testing "Example"
    (is (= 5934 (logic example 80))))

  (testing "Input"
    (is (= 362740 (logic input 80)))))

(deftest part2
  (testing "Example"
    (is (=  26984457539 (logic example 256))))

  (testing "Input"
    (is (= 1644874076764 (logic input 256)))))
