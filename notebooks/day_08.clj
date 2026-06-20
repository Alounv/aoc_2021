;; # Day 8
;; https://adventofcode.com/2021/day/8
(ns day-08
  (:require
   [aoc-2021.utils :refer [read-input]]
   [clojure.string :as str]))

;; ## Inputs
(def example "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def input (read-input 8))

;; ## Parsing
(defn parse-line [line]
  (let [[patterns digits] (str/split line #"\s+\|\s+")]
    [(str/split patterns #" ")
     (str/split digits #" ")]))

(defn parse [input]
  (let [lines (str/split-lines input)]
    (map parse-line lines)))

(def parsed-exemple (parse example))

;; ## Part 1
(defn count-easy-digits [parsed]
  (->> parsed
       (mapcat second) ;; mapcat second to get all digits
       (filter #(#{2 3 4 7} (count %)));; 1(2) 4(4) 7(3) 8(7)
       (count)))

(def answer-1-example (count-easy-digits parsed-exemple))
(def answer-1 (count-easy-digits (parse input)))

;; ## Part 2

(defn to-char-set [s] (set (str/split s #"")))

(defn get-code [patterns]
  (let [one (to-char-set (first (filter #(= 2 (count %)) patterns)))
        four (to-char-set (first (filter #(= 4 (count %)) patterns)))
        seven (to-char-set (first (filter #(= 3 (count %)) patterns)))
        eight (to-char-set (first (filter #(= 7 (count %)) patterns)))
        two-three-five (map #(to-char-set %) (filter #(= 5 (count %)) patterns)) ;; 2 3 5
        three (first (filter #(= 2 (count (filter one %))) two-three-five)) ;; only 3 overlaps with both segments of one
        five (first (filter #(= 3 (count (filter four %))) (remove #{three} two-three-five))) ;; of the remaining two, 5 overlaps with 3 segments of four (2 only overlaps 2)
        two (first (remove #{three five} two-three-five))
        zero-six-nine (map #(to-char-set %) (filter #(= 6 (count %)) patterns)) ;; 0 6 9
        six (first (filter #(= 1 (count (filter one %))) zero-six-nine)) ;; only 6 overlaps with just one segment of one (it lacks top-right)
        nine (first (filter #(= 4 (count (filter four %))) zero-six-nine)) ;; only 9 overlaps with all four segments of four
        zero (first (remove #{six nine} zero-six-nine))]
    {zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9}))

(defn compute-line [parsed]
  (let [code (get-code (first parsed))]
    (->> parsed
         (second)
         (map to-char-set)
         (map #(code %))
         (map str)
         (str/join)
         (Integer/parseInt))))

(defn compute [lines]
  (reduce + (map compute-line lines)))

(def answer-2-example (compute parsed-exemple))
(def answer-2 (compute (parse input)))

;; ## Regression checks

(assert (= answer-1-example 26))
(assert (= answer-1 288))

(assert (= answer-2-example 61229))
(assert (= answer-2 940724))
