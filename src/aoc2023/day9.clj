(ns aoc2023.day9
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [clojure.math.numeric-tower :as mnt]
            )
  )

(defn parse-ints
  [l]
  (map u/coerce-numeric (s/split l #" ")))

(def data
  (->> (ju/file-lines "data/day9.txt")
       (map parse-ints)))

;;; Part 1

(defn differences1
  [v]
  (mapv - (rest v) v))

;;; Needs a vector, if given a list it is a no-op
(defn extrapolated
  [v]
  (if (every? zero? v)
    (conj v 0)
    (conj v (+ (last v) (last (extrapolated (differences1 v)))))))

(def test
  [[0 3 6 9 12 15]
   [1 3 6 10 15 21]
   [10 13 16 21 30 45]])

(defn score
  [data]
  (reduce + (map (comp last extrapolated vec) data)))
        
(defn lconj
  [v e]
  (vec (cons e v)))

;;; Part 2

(defn extrapolated2
  [v]
  (if (every? zero? v)
    (conj v 0)
    (lconj v (- (first v) (first (extrapolated2 (differences1 v)))))))

(defn score2
  [data]
  (reduce + (map (comp first extrapolated2 vec) data)))
