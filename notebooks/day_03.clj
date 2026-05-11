;; # 💡 Day 3: Binary Diagnostic
;; https://adventofcode.com/2021/day/3
(ns day-03
  (:require
   [aoc-2021.utils :refer [get-lines parse-bit read-input]]
   [clojure.string :as str]))

;; ## Inputs
(def example
  "00100
  11110
  10110
  10111
  10101
  01111
  00111
  11100
  10000
  11001
  00010
  01010")

(def input (read-input 3))

;; ## Parsing
;; Each line is a binary string; we collect them as-is.
(defn parse [input]
  (->> input
       get-lines))

(def parsed-example (parse example))
(def parsed-input (parse input))

;; ## Part 1
;; For each bit position we want the *most common* bit across all lines —
;; that gives us the gamma rate. Epsilon is its bitwise complement. The
;; answer is `gamma * epsilon`.

;; Step 1: transpose the lines so each entry is one bit column.
(defn columns [bits]
  (apply map vector bits))

(def example-columns (columns parsed-example))

;; Step 2: count occurrences of each character per column.
(def example-frequencies (map frequencies example-columns))

;; Step 3: pick the most common character per column and join.
(defn most-common-bits [bits]
  (->> (columns bits)
       (map frequencies)
       (map #(apply max-key val %))
       (map key)
       (apply str)))

(def example-gamma (most-common-bits parsed-example))

;; Step 4: invert each bit to get epsilon.
(defn invert-bits [s]
  (str/replace s #"\d" {"0" "1" "1" "0"}))

(def example-epsilon (invert-bits example-gamma))

;; Putting it together:
(defn logic-1 [bits]
  (let [gamma (most-common-bits bits)
        epsilon (invert-bits gamma)]
    (* (parse-bit gamma) (parse-bit epsilon))))

(def answer-1-example (logic-1 parsed-example))
(def answer-1-input (logic-1 parsed-input))

;; ## Part 2
;; For the oxygen rating we repeatedly keep only the lines whose bit at
;; position `i` matches the *majority* bit at that position. For CO2 we
;; keep the *minority*. We advance `i` until only one line remains.

;; Split bits into two buckets: those whose i-th char is '0' (`Z`) and those
;; whose i-th char is '1' (`O`).
(defn partition-in-Z-O [bits i]
  (reduce (fn [[Z O] bit]
            (if (= (nth bit i) \0)
              [(conj Z bit) O]
              [Z (conj O bit)]))
          [[] []]
          bits))

;; Order them so the bigger bucket is first. Ties go to `O` (the '1' bucket),
;; which matches the puzzle's tie-breaking rule.
(defn get-maj-min [Z O]
  (if (> (count Z) (count O))
    [Z O]
    [O Z]))

;; Filter loop: at each step keep the majority (or minority) bucket and
;; advance one bit position. Stops when a single line is left.
(defn filter-by [bits criteria]
  (loop [bits bits i 0]
    (if (= 1 (count bits))
      (first bits)
      (let [[Z O] (partition-in-Z-O bits i)
            [maj min] (get-maj-min Z O)
            next (if (= criteria :maj) maj min)]
        (recur next (inc i))))))

(defn logic-2 [bits]
  (let [oxygen (filter-by bits :maj)
        co2 (filter-by bits :min)]
    (* (parse-bit oxygen) (parse-bit co2))))

(def answer-2-example (logic-2 parsed-example))
(def answer-2-input (logic-2 parsed-input))

;; ## Regression checks
(assert (= 198 answer-1-example))
(assert (= 1082324 answer-1-input))
(assert (= 230 answer-2-example))
(assert (= 1353024 answer-2-input))
