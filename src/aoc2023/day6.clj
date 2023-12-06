(ns aoc2023.day6
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

;;; Too simple

(def times [48 93 84 66])
(def distances [261 1192 1019 1063])

(def races (map vector times distances))

(defn race-distance
  [time speed]
  (* (- time speed) speed))

(defn count-ways
  [[time distance]]
  (count (filter #(> (race-distance time %) distance) (range time))))

(defn part1 []
  (reduce * (map count-ways races)))

(defn part2 []
  (count-ways [48938466 261119210191063]) )

