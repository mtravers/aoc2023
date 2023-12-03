(ns aoc2023.day3
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

(def array (vec (ju/file-lines "data/day3.txt")))

#_
(def array
  ["467..114.."
   "...*......"
   "..35..633."
   "......#..."
   "617*......"
   ".....+.58."
   "..592....."
   "......755."
   "...$.*...."
   ".664.598.."])

(defn achar [l i]
  (get (get array l) i))

(defn astring [l i j]
  (subs (get array l) i j))

(def nonsymbols (conj (set "0123456789.") nil))
(def digits (set "0123456789"))

(defn symbol? [char]
  (not (contains? nonsymbols char)))

(defn numeric? [char]
  (contains? digits char))


(defn in-bounds? [[l i]]
  (and (<= 0 l (count array))
       (<= 0 i (count (first array)))))  ;assumes all lines are the same size
  
(defn neighbors [l i j]
  (filter in-bounds?
          (concat [[l (dec i)]
                   [l j]]
                  (mapcat (fn [k] [[(dec l) k]
                                   [(inc l) k]])
                          (range (dec i) (inc j))))))

(defn num-seq
  [line]
  (loop [num-strings (re-seq #"\d+" line)
         result []
         finger 0]
    (if (empty? num-strings)
      result
      (let [num-string (first num-strings)
            pos (s/index-of line num-string finger)]
        (recur (rest num-strings)
               (conj result [pos (+ pos (count num-string))])
               (+ pos (count num-string)))))))

(defn process-line
  [l]
  (u/forf [[start end] (num-seq (get array l))]
    (when (some (fn [[i j]] (symbol? (achar i j))) (neighbors l start end))
      (u/coerce-numeric (astring l start end)))))
    
    
(reduce + (mapcat process-line (range (count array))))


;;; Part 2

(defn consolidate-neighbors
  [neighbors]
  (loop [tail neighbors
         result (set neighbors)]
    (if (empty? tail)
      (into [] result)
      (let [[i j] (first tail)]
        (recur (rest tail)
               (disj result [i (dec j)]))))))
         
(defn number-at
  [l i]
  (let [start (+ 1 (or (u/some-thing #(not (numeric? (achar l %))) (range i -1 -1)) -1))
        end (or (u/some-thing #(not (numeric? (achar l %))) (range i (count (first array)))) (count (first array)))]
    (u/coerce-numeric (astring l start end))))


(defn process-line-2
  [l]
  (let [line (get array l)]
    (u/forf [i (range (count line)) ]
      (if (symbol? (achar l i))
        (let [neighbors (neighbors l i (inc i))
              numeric-neighbors (filter (fn [[i j]] (numeric? (achar i j))) neighbors)
              consolidated-neighbors (consolidate-neighbors numeric-neighbors)]
          (when (= (count consolidated-neighbors) 2)
            (* (apply number-at (first consolidated-neighbors))
               (apply number-at (second consolidated-neighbors)))))))))


          
          
(reduce + (mapcat process-line-2 (range (count array))))
