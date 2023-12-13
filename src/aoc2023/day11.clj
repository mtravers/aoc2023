(ns aoc2023.day11
  (:require [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            )
  )

;;; Data

(def data (-> "data/day11.txt"
              ju/file-lines
              vec))

(def test
  ["...#......"
   ".......#.."
   "#........."
   ".........."
   "......#..."
   ".#........"
   ".........#"
   ".........."
   ".......#.."
   "#...#....."])


;;; Part 1

;;; Multitool? Surely something like this exists
(defn indices
  [p seq]
  (filter identity (map-indexed (fn [i e] (when (p e) i)) seq)))

;;; Test (indices even? [2 10 3 8 7 11 0])

(defn char-at
  [data [i j]]
  (get-in data [j i]))

(defn part1
  [data]
  (let [empty-rows (indices #(re-matches  #"\.*" %) data)
        empty-cols (u/mapf (fn [col]
                             (when (every? (fn [line] (= \. (nth line col))) data)
                               col))
                           (range (count (first data))))
        galaxies (u/forf [i (range (count (first data)))
                          j (range (count data))]
                   (let [p [i j]]
                     (when (= \# (char-at data p))
                       [(+ i (count (filter #(< % i) empty-cols)))
                        (+ j (count (filter #(< % j) empty-rows)))
                        ]
                       )))
        distance (fn [[i0 j0] [i1 j1]]
                   (+ (abs (- i0 i1)) (abs (- j0 j1))))
        all-distances (u/forf [g0 galaxies g1 galaxies]
                        (when (u/<* g0 g1)
                          (distance g0 g1)))]
    (reduce + all-distances)))
    
;;; Part 2

;;; Hah, this was a trivial change from the above
(defn part2
  [data]
  (let [empty-rows (indices #(re-matches  #"\.*" %) data)
        empty-cols (u/mapf (fn [col]
                             (when (every? (fn [line] (= \. (nth line col))) data)
                               col))
                           (range (count (first data))))
        galaxies (u/forf [i (range (count (first data)))
                          j (range (count data))]
                   (let [p [i j]]
                     (when (= \# (char-at data p))
                       [(+ i (* 999999 (count (filter #(< % i) empty-cols))))
                        (+ j (* 999999 (count (filter #(< % j) empty-rows))))
                        ]
                       )))
        distance (fn [[i0 j0] [i1 j1]]
                   (+ (abs (- i0 i1)) (abs (- j0 j1))))
        all-distances (u/forf [g0 galaxies g1 galaxies]
                        (when (u/<* g0 g1)
                          (distance g0 g1)))]
    (reduce + all-distances)))
