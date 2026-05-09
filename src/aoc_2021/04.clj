(ns aoc-2021.04
  (:require
   [aoc-2021.utils :refer [get-blocks get-lines read-input]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

;; Parse

(defn parse-line [line]
  (let [v (as-> line $
            (str/trim $)
            (str/split  $ #"[\s,]+")
            (map #(Integer/parseInt %) $))]
    (vec v)))

(defn parse-boards [block]
  (->> block
       get-lines
       (mapv parse-line)))

(defn parse [input]
  (let [[wins & boards] (get-blocks input)
        wins  (parse-line wins)
        boards (map parse-boards boards)]
    [wins boards]))

;; Logic

(defn is_a_win [drawn board] ;; drawn is a set
  (let [d-set (set drawn)
        rows board
        cols (apply mapv vector board)]
    (or (some #(every? d-set %) rows)
        (some #(every? d-set %) cols))))

(defn find_a_win [drawn boards]
  (->> boards
       (filter #(is_a_win drawn %))
       first))

(defn get_score [drawn board]
  (let [all (set (flatten board))
        losing (set/difference all (set drawn))
        losing-sum (reduce +  losing)]
    (* (last drawn) losing-sum)))

(defn get_result [numbers boards]
  (->>
   (inc (count numbers))
   (range 1)
   (some (fn [i]
           (let [drawn (take i numbers)]
             (when-let [win (find_a_win drawn boards)]
               (get_score drawn win)))))))

(defn logic_1 [input]
  (->> input
       parse
       (apply get_result)))

;; Logic 2

(defn find_a_win_but_only_last [drawn boards]
  (->> boards
       (filter #(is_a_win drawn %))
       (filter #(not (is_a_win (butlast drawn) %))) ;; it should only be a win with the last number
       first))

(defn get_result_2 [numbers boards]
  (as-> numbers $
    (inc (count $))
    (range $ 1 -1)
    (some (fn [i]
            (let [drawn (take i numbers)]
              (when-let [win (find_a_win_but_only_last drawn boards)]
                (get_score drawn win)))) $)))

(defn logic_2 [input]
  (->> input
       parse
       (apply get_result_2)))

;; Inputs

(def input (read-input 4))

(def example
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

  22 13 17 11  0
   8  2 23  4 24
  21  9 14 16  7
   6 10  3 18  5
   1 12 20 15 19

   3 15  0  2 22
   9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6

  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
   2  0 12  3  7")

;; Tests

(deftest part1
  (testing "Example"
    (is (= 4512 (logic_1 example))))

  (testing "Input"
    (is (= 87456 (logic_1 input)))))

(deftest part2
  (testing "Example"
    (is (= 1924 (logic_2 example))))

  (testing "Input"
    (is (= 15561 (logic_2 input)))))
