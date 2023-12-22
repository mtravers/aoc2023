(ns aoc2023.day13
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [clojure.math.numeric-tower :as mnt]
            )
  )

;;; → Multitool,

;;; not performant, not used
(defn group-indicies
  [seq]
  (u/map-values
   (fn [l] (mapv second l))
   (group-by first (map-indexed (fn [i e] [e i]) seq))))

;;; Transpose a vector of strings.
(defn transpose-strings
  [g]
  (mapv (fn [i]
          (apply str
                 (map (fn [s]
                        (nth s i))
                      g)))
        (range (count (first g)))))

;;; Data

(def data
  (->> "data/day13.txt"
       ju/file-lines
       (partition-by empty?)
       (remove #(= '("") %))))


;;; TODO filter out evens? 
(defn find-reflections-0
  [g]
  (let [row0 (first g)
        reflect0s (rest (filter identity (map-indexed (fn [i row] (if (= row0 (nth g i)) i nil)) g)))
        ]
    (u/mapf (fn [right]
              (when (every? (fn [col] (= (nth g col) (nth g (- right col))))
                            (range 1 (/ (+ right 1) 2)))
                (/ (+ right 1) 2)))
          reflect0s)))

(defn find-reflections-1
  [g]
  (concat (find-reflections-0 g)
          (map #(- (count g) %) (find-reflections-0 (reverse g)))))


(defn find-reflections
  [g]
  [(find-reflections-1 g)
   (find-reflections-1 (transpose-strings g))])

(defn part1
  []
  (+ (* 100 (reduce + (mapcat find-reflections-1 data)))
     (reduce + (mapcat (comp find-reflections-1 transpose-strings) data))))
     
;;; Tests

(def t0
  ["#.##..##."
   "..#.##.#."
   "##......#"
   "##......#"
   "..#.##.#."
   "..##..##."
   "#.#.##.#."])

(def t1
  ["#...##..#"
   "#....#..#"
   "..##..###"
   "#####.##."
   "#####.##."
   "..##..###"
   "#....#..#"])

(def t3 '("###.###..###."
   "###.###..###."
   ".#...#.##.#.."
   ".#.#..####..#"
   ".#...##..###."
   "#.###########"
   "#####......##"))

;;; Part 2 Brute force

(defn part2-brute
  [g]
  (prn :original (find-reflections g))
  (let [original (mapv vec g)]
    (for [i (range (count (first g)))
          j (range (count g))]
      (let [mutated (update-in original [j i] #(if (= % \#) \. \#))]
        [i j (find-reflections mutated)]))))
