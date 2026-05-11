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
(defn older
  "Make fish grow old, reproducing if necessary"
  [fish]
  (if (zero? fish) [6 8] [(dec fish)]))

(older 6) ;; just grow old
(older 0) ;; grow old and reproduce

(defn evolve
  "Flatten the result of applying 'older' to each fish"
  [fishes]
  (mapcat older fishes))

(evolve parsed-example)

(defn evolve-n
  "See what happens after n generations"
  [n fishes]
  (nth (iterate evolve fishes) n))

(evolve-n 3 parsed-example)

(defn logic-1 [input]
  (->> input
       (evolve-n 80)
       (count)))

(def answer-1-example (logic-1 parsed-example))
(def answer-1-input (logic-1 parsed-input))

;; ## Part 2
;;
;; We use a frequency map to track the number of fish by age
(frequencies parsed-input)

(defn f-evolve
  "Evolve the frequency of fish by age"
  [by-age]
  {0 (by-age 1 0),
   1 (by-age 2 0),
   2 (by-age 3 0),
   3 (by-age 4 0),
   4 (by-age 5 0),
   5 (by-age 6 0),
   6 (+ (by-age 0 0) (by-age 7 0)), ;; (7 = new generation) / (0 = old fishes)
   7 (by-age 8 0),
   8 (by-age 0 0)})

(f-evolve {3 2, 4 1})

(defn f-evolve-n [n by-age]
  (nth (iterate f-evolve by-age) n))

(defn logic-2 [input]
  (->> input
       (frequencies)
       (f-evolve-n 256)
       (vals)
       (reduce +)))

(def answer-2-example (logic-2 parsed-example))
(def answer-2-input (logic-2 parsed-input))

;; ## Regression checks
(assert (= 5934 answer-1-example))
(assert (= 348729 answer-1-input))
(assert (= 26984457539 answer-2-example))
(assert (= 1580996496278 answer-2-input))
