(ns aoc2023.day10
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [clojure.math.numeric-tower :as mnt]
            )
  )

;;; Data

(def data
  (->> (ju/file-lines "data/day10.txt")
       vec))

(def code
  {\| [[0 -1] [0 1]]
   \- [[-1 0] [1 0]]
   \L [[0 -1] [1 0]]
   \J [[0 -1] [-1 0]]
   \7 [[0 1] [-1 0]]
   \F [[0 1] [1 0]]
   \S [[0 1] [0 -1] [1 0] [-1 0]]       ;iffy
   })
  ;;. is ground; there is no pipe in this tile.
;; S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.])

(def +* (u/vectorize +))

(defn char-at
  [data [i j]]
  (get-in data [j i]))

;;; Part 1

(defn moves
  [data p]
  (let [c (char-at data p)
        deltas (get code c)
        neighbors (map #(+* p %) deltas)]
    ;; filters out of bounds; should perhaps filter on actual content
    (filter #(char-at data %) neighbors)))

(defn start
  [data]
  (some (fn [j]
          (when-let [i (s/index-of (get data j) \S)]
            [i j]))
        (range (count data))))

;;; AKA lcontains?
(defn some=
  [v seq]
  (some #(= v %) seq))

(defn some-uneq-thing
  [v seq]
  (some #(and (not (= v %)) %) seq))

(defn starting-pipes
  [data]
  (let [s (start data)]
    (filter (fn [p] (some= s (moves data p)))
            (moves data s))))
        
;;; Note: this could probably be simplified a bit
(defn follow
  [data start from]
  (loop [last start
         at from
         path [start from]]
    (if (= \S (char-at data at))
      path
      (let [next (some-uneq-thing last (moves data at))]
        (recur at
               next
               (conj path next))))))

(defn solve
  [data]
  (let [start (start data)
        starting-pipe (first (starting-pipes data))
        path (follow data start starting-pipe)]
    (/ (dec (count path)) 2)))

;;; Tests

(def test1
  ["..F7."
   ".FJ|."
   "SJ.L7"
   "|F--J"
   "LJ..."])

