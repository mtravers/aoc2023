(ns aoc2023.day1
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

(def items (ju/file-lines "data/day1.txt"))

;;; Part 1

(defn extract-number
  [item]
  (let [digits (re-seq #"\d" item)]
    (+ (* 10 (u/coerce-numeric (first digits)))
       (u/coerce-numeric (last digits)))))

(reduce + (map extract-number items))  

;;; Part 2

(defn coerce-harder
  [thing]
  (or ({"one" 1
        "two" 2
        "three" 3
        "four" 4
        "five" 5
        "six" 6
        "seven" 7
        "eight" 8
        "nine" 9} thing)
      (u/coerce-numeric thing)))

;;; Match subseqs with overlap. Surprisingly hard to do and requires that the regex have start and end anchors. Not performant or I would say a candidate for multitool
;;; Also, probably don't need to do this, easier to find first and last separately
(defn re-seq-x
  [re s]
  (u/forf [start (range (count s))]
    (let [substring (subs s start)]
      (second (re-matches re substring)))))

(defn extract-number-2
  [item]
  (let [digits (re-seq-x #"^(\d|one|two|three|four|five|six|seven|eight|nine).*$" item)]
    (+ (* 10 (coerce-harder (first digits)))
       (coerce-harder (last digits)))))

(reduce + (map extract-number-2 items)) 
