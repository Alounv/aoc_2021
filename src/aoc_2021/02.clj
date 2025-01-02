(ns aoc-2021.02
  (:require
   [aoc-2021.utils :refer [get-lines read-input]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

;; Parse

(defrecord Move [dx dy])

(defn parse-line [line]
  (let [[dir d] (str/split line #" ")
        d (Integer/parseInt d)
        dx (case dir "forward" d "backward" (- d) 0)
        dy (case dir "down" d "up" (- d) 0)]
    (->Move dx dy)))

(defn parse [input]
  (->> input
       get-lines
       (map parse-line)))

;; Logic

(defn sum-moves [moves]
  (let [total-dx (reduce + (map :dx moves))
        total-dy (reduce + (map :dy moves))]
    (* total-dx total-dy)))

(defn logic_1 [input]
  (->> input
       parse
       sum-moves))

;; Logic 2

(defrecord P [x y])

(defn move [moves]
  (loop [aim 0 position (->P 0 0) moves moves]
    (let [x (:x position) y (:y position)]
      (if (empty? moves)
        (* x y)
        (let [m (first moves)
              d_x (:dx m)
              d_aim (:dy m)
              new-aim (+ aim d_aim)
              new-pos (->P (+ x d_x)
                           (+ y (* d_x aim)))]
          (recur new-aim new-pos (rest moves)))))))

(defn logic_2 [input]
  (->> input
       parse
       move))

;; Inputs

(def input (read-input 2))

(def example
  "forward 5
  down 5
  forward 8
  up 3
  down 8
  forward 2")

;; Tests

(deftest part2
  (testing "Example"
    (is (= 150 (logic_1 example))))

  (testing "Input"
    (is (= 1893605 (logic_1 input)))))

(deftest part2
  (testing "Example"
    (is (= 900 (logic_2 example))))

  (testing "Input"
    (is (= 2120734350
           (logic_2 input)))))
