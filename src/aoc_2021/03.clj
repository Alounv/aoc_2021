(ns aoc-2021.03
  (:require
   [aoc-2021.utils :refer [get-lines parse-bit read-input]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

;; Parse

(defn parse [input]
  (->> input
       get-lines))

;; Logic

(defn not-binary [s]
  (str/replace s #"\d" {"0" "1" "1" "0"}))

(defn majority-bits [bits]
  (->> (apply map vector bits) ;; transpose to get columns
       (map frequencies)
       (map #(apply max-key val %))
       (map key)
       (apply str)))

(defn calc [bits]
  (let [maj (majority-bits bits) min (not-binary maj)]
    (* (parse-bit maj) (parse-bit min))))

(defn logic_1 [input]
  (->> input
       parse
       calc))

;; Part 2

(defn partition-in-Z-O
  "Partitions bits in Z and O depending on the value of the i-th bit"
  [bits i]
  (reduce (fn [[Z O] bit]
            (if (= (nth bit i) \0)
              [(conj Z bit) O]
              [Z (conj O bit)]))
          [[] []]
          bits))

(defn get-maj-min
  "Put Z and O in order of majority and minority"
  [Z O]
  (if (> (count Z) (count O))
    [Z O]
    [O Z]))

(defn fd
  "Calculate depending on the criteria"
  [bits criteria]
  (loop [bits bits i 0]
    (if (= 1 (count bits))
      (first bits) ;; one bit left, return it

      (let [[Z O] (partition-in-Z-O bits i)
            [maj min] (get-maj-min Z O) ;; get majority and minority
            next (if (= criteria "maj") maj min)] ;; keep only majority or minority

        (recur next (inc i)))))) ;; loop

(defn calc_2 [bits]
  (let [maj (fd bits "maj")
        min (fd bits "min")]
    (* (parse-bit maj) (parse-bit min))))

(defn logic_2 [input]
  (->> input
       parse
       calc_2))

;; Inputs

(def input (read-input 3))

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

;; Tests

(deftest part1
  (testing "Example"
    (is (= 198 (logic_1 example))))

  (testing "Input"
    (is (= 1082324 (logic_1 input)))))

(deftest part2
  (testing "Example"
    (is (= 230 (logic_2 example))))

  (testing "Input"
    (is (= 1353024 (logic_2 input)))))
