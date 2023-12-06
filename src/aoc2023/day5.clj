(ns aoc2023.day5
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

;;; ◡◠ Data ◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠◡◠

(defn parse-num-list
  [l]
  (map u/coerce-numeric (s/split l #" +" )))

(defn parse-num-vec
    [l]
  (vec (parse-num-list l)))

(defn read-data
  []
  (let [groups (->> "data/day5.txt"
                    ju/file-lines
                    (u/partition-if empty?))
        seeds (rest (parse-num-list (second (s/split (ffirst groups)  #"\:"))))
        maps (for [g (rest groups)]
               (map parse-num-vec (drop 2 g))
               )]
    (cons seeds maps)))

(def data (read-data))
(def seeds (first data))
(def maps (rest data))

(defn map-entry?
  [n [dest-start source-start length]]
  (if (<= source-start n (+ source-start length -1))
    (+ dest-start (- n source-start))))

(defn map-n
  [n map]
  (or (some (partial map-entry? n) map)
      n))

(defn trace-seed
  [seed]
  (loop [current seed
         maps maps]
    (if (empty? maps)
      current
      (recur (map-n current (first maps))
             (rest maps)))))

(defn part1
  []
  (apply min (map trace-seed seeds)))

;;; Part 2

;;; Naive implementation
;;; Doesn't work at all, expand-seeds is too much
;;; Needs some kind of range arithetic I guess, wotta pain

(defn expand-seeds
  [seeds]
  (mapcat (fn [[s l]] (range s (+ s l))) (partition 2 seeds)))

(defn part2
  []
  (apply min (map trace-seed (expand-seeds seeds))))

;;; Slightly less naive, still useless
(defn part2a
  []
  (let [find-min (fn [[s l]]
                   (let [e (+ s l)]
                   (loop [i s
                          min java.lang.Long/MAX_VALUE]
                     (if (= i e) min
                         (let [n (trace-seed i)]
                           (recur (inc i)
                                  (if (< n min) n min)))))))]
    (pmap find-min (partition 2 seeds))))

;;; Range version

(defn map-entry?
  [[seed-start seed-length] [dest-start source-start length]]
  (let [seed-end (+ seed-start seed-length)
        source-end (+ source-start length)]
    (cond (< seed-end source-start) nil
          (> seed-start source-end) nil
          (<= seed-start source-start)
          [source-start ]


  (if (<= source-start n (+ source-start length -1))
    (+ dest-start (- n source-start))))

(defn map-n-range
  [[start count] map]
  (for [[dest-start source-start length]] map)

(defn trace-seed-range
  [[start count]]
  (loop [current (vec seed)]
         maps maps]
    (if (empty? maps)
      current
      (recur (mapcat map-n-range current (first maps)) ;TODO not quite
             (rest maps)))))
