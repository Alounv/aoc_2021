;; # 🐟 Day 6: Lanternfish

(ns day-06
  (:require
   [aoc-2021.utils :refer [parse-int read-input]]
   [clojure.string :as str]))

;; ## Inputs
(def example "3,4,3,1,2")

(def input (read-input 6))

;; ## Parsing
(defn parse [input]
  (->>
   (str/split input #",")
   (map parse-int)))

(def parsed-example (parse example))
(def parsed-input (parse input))

;; ## Part 1
(defn older [fish]
  (if (zero? fish) [6 8] [(dec fish)]))

(older 6)
(older 0)

(defn evolve [fishes]
  (mapcat older fishes))

(evolve parsed-example)

(defn evolve-n [n fishes]
  (->> fishes
       (iterate evolve)
       (take (inc n))))

(evolve-n 3 parsed-example)

(defn logic-1 [input]
  (->> input
       (evolve-n 80)
       (last)
       (count)))

(def answer-1-example (logic-1 parsed-example))
(def answer-1-input (logic-1 parsed-input))

;; ## Part 2
(defn logic-2 [input]
  input)

(def answer-2-example (logic-2 parsed-example))
;; (def answer-2-input (logic-2 parsed-input))

;; ## Regression checks
(assert (= 5934 answer-1-example))
(assert (= 348729 answer-1-input))
;; (assert (= :TODO answer-2-example))
;; (assert (= :TODO answer-2-input))
