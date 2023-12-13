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
;; . is ground; there is no pipe in this tile.
;; S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.])

(def +* (u/vectorize +))
(def -* (u/vectorize -))

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

;;; Multitool
;;; AKA lcontains?
(defn some=
  [v seq]
  (some #(= v %) seq))

;;; Multitool? Needs a better name
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
    #_ (draw-path data path)
    (/ (dec (count path)) 2)))

(defn draw-path
  [data path]
  (let [pathset (set path)]
    (doseq [j (range (count data))
            i (range (count (first data)))]
      (when (= i 0) (newline))
      (print (if (contains? pathset [i j])
               (char-at data [i j]) \.)))))        
      

;;; Part 2

(def decode (set/map-invert code))

(defn normalize-s
  [data [i j :as spt] path]
  (let [n0 (-* (second path) spt)
        n1 (-* (nth path (- (count path) 2)) spt)]
    (or (get decode [n0 n1])
        (get decode [n1 n0]))))

(defn path
  [data]
  (let [start (start data)
        starting-pipe (first (starting-pipes data))
        path (follow data start starting-pipe)]
    path))

;;; Candidate for multitool, needs better name
;;; Based on clojure.core/re-seq
(defn re-seq-p
  "Returns a lazy sequence of successive matches of pattern in string, returning [start end] pairs"
  [^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    (loop [result []]
      (if (.find m)
        (recur (conj result [(.start m 0) (.end m 0)]))
        result))))


;;; Returns a vector of strings (like original data) but only including the actual path, with S restored to a normal pipe.
(defn path-only-grid
  [data path]
  (map-indexed
   (fn [j line]
     (loop [[[i _ :as pt] & rest] (filter #(= (second %) j) path)
            array (vec (repeat (count (first data)) \.))]
       (if pt
         (let [raw-char (get line i)
               char (if (= raw-char \S) (normalize-s data pt path) raw-char)]
           (recur rest
                  (assoc array i char)))
         (apply str array)
         )))
   data))

;;; Count inside points using (essentially) the Jordan curve theorem to go line-by-line.
(defn count-innies
  [line]
  (loop [[wall & rest] (re-seq-p #"(F-*J|L-*7|\|)" line) ;find crossings
         count 0
         out? false]
    (if wall
      (recur rest
             (if out?
               count
               (+ count (get (frequencies (subs line (second wall) (ffirst rest))) \. 0)))
             (not out?))
      count)))

(defn solve2
  [data]
  (let [path (path data)
        grid (path-only-grid data path)]
    (reduce + (map count-innies grid))))

;;; Tests

(def test1
  ["..F7."
   ".FJ|."
   "SJ.L7"
   "|F--J"
   "LJ..."])

