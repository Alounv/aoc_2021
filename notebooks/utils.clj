;; # 🛠 Utils
;; Documentation and regression checks for the shared helpers defined in
;; `aoc-2021.utils`.
(ns utils
  (:require [aoc-2021.utils :as u]))

;; ## Points
;; Manhattan distance is the sum of axis-wise absolute differences.
(u/manhattan-distance [0 0] [2 4])

(assert (= 6 (u/manhattan-distance [0 0] [2 4])))

;; ## Grids
;; `grid-create` turns a multi-line string into a 2D vector of characters.
(u/grid-create "123\n456\n789")

(assert (= [[\1 \2 \3]
            [\4 \5 \6]
            [\7 \8 \9]]
           (u/grid-create "123\n456\n789")))

;; `grid-get` takes `[x y]` (column, then row).
(u/grid-get [[\1 \2 \3] [\4 \5 \6] [\7 \8 \9]] [0 0])

(assert (= \1 (u/grid-get [[\1 \2 \3]
                           [\4 \5 \6]
                           [\7 \8 \9]]
                          [0 0])))

;; `grid-list` walks the grid in row-major order and returns a flat seq.
(u/grid-list [[\1 \2 \3] [\4 \5 \6] [\7 \8 \9]])

(assert (= [\1 \2 \3 \4 \5 \6 \7 \8 \9]
           (u/grid-list [[\1 \2 \3]
                         [\4 \5 \6]
                         [\7 \8 \9]])))

;; `create-empty-grid` makes a `[my x mx]` grid filled with `nil`.
(u/create-empty-grid 3 3)

(assert (= [[nil nil nil]
            [nil nil nil]
            [nil nil nil]]
           (u/create-empty-grid 3 3)))

;; ## Stringify
;; A small dispatcher for printing different shapes:
(assert (= "7" (u/stringify 7)))
(assert (= "MyName" (u/stringify :MyName)))
(assert (= "{:a 7, :b 8}" (u/stringify {:a 7 :b 8})))
(assert (= "7, 4, 9, 5, 11, 17" (u/stringify [7 4 9 5 11 17])))
(assert (= "1, 2, 3, 4, 5, 6" (u/stringify (range 1 7))))
(assert (= "...\n...\n..." (u/stringify (u/create-empty-grid 3 3))))
(assert (= "123\n456\n789" (u/stringify (u/grid-create "123\n456\n789"))))
