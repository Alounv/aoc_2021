;; # 🎱 Day 4: Giant Squid
;; https://adventofcode.com/2021/day/4
(ns day-04
  (:require
   [aoc-2021.utils :refer [get-blocks get-lines read-input]]
   [clojure.set :as set]
   [clojure.string :as str]))

;; ## Inputs
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

(def input (read-input 4))

;; ## Parsing
;; The input is one block of draws (comma-separated numbers) followed by
;; one or more blocks of 5x5 boards (rows of whitespace-separated numbers).
;; We return `[draws boards]`.
(defn parse-line [line]
  (->> (str/split (str/trim line) #"[\s,]+")
       (mapv #(Integer/parseInt %))))

(defn parse-board [block]
  (->> block
       get-lines
       (mapv parse-line)))

(defn parse [input]
  (let [[draws & boards] (get-blocks input)]
    [(parse-line draws)
     (mapv parse-board boards)]))

(def parsed-example (parse example))
(def parsed-input (parse input))

;; ## Part 1
;; Play the draws in order; the first board to complete a full row or
;; column wins. The score is the sum of the unmarked numbers on that
;; board, multiplied by the last drawn number.

;; A board wins if any row OR any column is fully covered by the drawn set.
(defn winning-board? [drawn-set board]
  (let [rows board
        cols (apply mapv vector board)]
    (or (some #(every? drawn-set %) rows)
        (some #(every? drawn-set %) cols))))

(defn find-winner [drawn boards]
  (let [drawn-set (set drawn)]
    (first (filter #(winning-board? drawn-set %) boards))))

(defn score [drawn board]
  (let [all (set (flatten board))
        unmarked (set/difference all (set drawn))]
    (* (last drawn) (reduce + unmarked))))

;; Increase the number of drawn balls one at a time; stop at the first
;; turn that produces a winning board.
(defn play-until-first-win [[numbers boards]]
  (->> (range 1 (inc (count numbers)))
       (some (fn [i]
               (let [drawn (take i numbers)]
                 (when-let [win (find-winner drawn boards)]
                   (score drawn win)))))))

(def answer-1-example (play-until-first-win parsed-example))
(def answer-1-input (play-until-first-win parsed-input))

;; ## Part 2
;; Now we want the *last* board to win. We sweep turns from largest to
;; smallest and look for a board that wins on this turn but did not win
;; on the previous turn — that's a board winning exactly now. The first
;; such board (counting down from the end) is the last one to win.
(defn find-newly-won-board [drawn boards]
  (let [now (set drawn)
        before (set (butlast drawn))]
    (->> boards
         (filter #(winning-board? now %))
         (remove #(winning-board? before %))
         first)))

(defn play-until-last-win [[numbers boards]]
  (->> (range (inc (count numbers)) 1 -1)
       (some (fn [i]
               (let [drawn (take i numbers)]
                 (when-let [win (find-newly-won-board drawn boards)]
                   (score drawn win)))))))

(def answer-2-example (play-until-last-win parsed-example))
(def answer-2-input (play-until-last-win parsed-input))

;; ## Regression checks
(assert (= 4512 answer-1-example))
(assert (= 87456 answer-1-input))
(assert (= 1924 answer-2-example))
(assert (= 15561 answer-2-input))
