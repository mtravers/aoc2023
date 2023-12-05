(ns aoc2023.day4
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

;;; Other nice solutions
;;; https://github.com/ckainz11/AdventOfCode2023/blob/main/src/main/kotlin/days/day04/Day4.kt

;;; ◡◠ Data ◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠

(def cards (ju/file-lines "data/day4.txt"))

#_
(def cards
  ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(defn parse-num-list
  [l]
  (map u/coerce-numeric (s/split l #" +" )))

(defn parse-num-set
    [l]
  (set (parse-num-list l)))

(defn parse-card-line
  [l]
  (let [[_ card wins haves] (re-matches #"Card +(\d+): +(.*)\| (.*)$" l)]
    [(u/coerce-numeric card)
     (parse-num-set wins)
     (parse-num-set haves)]))
        
(defn card-matches
  [[card wins haves]]
  (count (set/intersection wins haves)))  

;;; ◡◠ Part 1 ◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠

(defn score-card-line
  [card]
  (int (Math/pow 2 (dec (card-matches card)))))

(defn part1
  []
  (reduce + (map (comp score-card-line parse-card-line) cards)))

;;; ◡◠ Part 2 ◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠

(defn part2
  []
  (loop [counts (repeat (count cards) 1)
         i 0
         total 0]
    (if (empty? counts)
      total
      (let [score (card-matches (parse-card-line (nth cards i)))
            card-count (first counts)]
        (recur (map + (rest counts) (concat (repeat score card-count) (repeat 0)))
               (inc i)
               (+ total card-count))))))
    

