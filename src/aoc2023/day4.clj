(ns aoc2023.day4
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

;;; Other nice solutions
;;; https://github.com/ckainz11/AdventOfCode2023/blob/main/src/main/kotlin/days/day04/Day4.kt


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
  (let [raw (map u/coerce-numeric (s/split l #" +" ))]
    (set raw)))

(defn parse-card-line
  [l]
  (let [[_ card wins haves] (re-matches #"Card +(\d+): +(.*)\| (.*)$" l)]
    [(u/coerce-numeric card)
     (parse-num-list wins)
     (parse-num-list haves)]))
        
(defn score-card-line
  [[card wins haves]]
  (int (Math/pow 2 (dec (count (clojure.set/intersection wins haves))))))

(reduce + (map (comp score-card-line parse-card-line) cards))

;;; Part 2

(defn score-card-line-2
  [[card wins haves]]
  (count (clojure.set/intersection wins haves)))

(u/defn-memoized card-base-score
    [card-n]
  (score-card-line-2
   (parse-card-line
    (nth cards card-n))))

;;; Not working dammit, not sure why. Does OK on test case.
(defn total
  []
  (loop [counts (repeat (count cards) 1)
         i 0
         total 0]
    (if (empty? counts)
      total
      (let [score (card-base-score i)
            card-count (first counts)]
        (prn :x i score card-count total counts)
        (recur (map + (rest counts) (concat (repeat score card-count) (repeat 0)))
               (inc i)
               (+ total card-count))))))
    
