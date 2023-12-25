(ns aoc2023.day15
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [clojure.math.numeric-tower :as mnt]
            [nextjournal.clerk :as clerk]
            )
  )

(defn hash-string
  [s]
  (loop [h 0
         i 0]
    (if (= i (count s))
      h
      (recur (mod (* (+ h (int (nth s i))) 17) 256)
             (inc i)))))

(defn hash-strings
  [s]
  (reduce + (map hash-string (s/split s #"[,\n]"))))

(defn data
  []
  (slurp "data/day15.txt"))

;;; 504411 too low
;;; newline was the problem
;;; 504449

;;; Part 2 (ok this seems kinda dumb)

(defn replace-or-add-lens
  [lenses code num]
  (if-let [pos (u/position= code lenses first)]
    (assoc-in lenses [pos 1] num)
    (conj lenses [code num])))

(defn delete-lens
  [lenses code num]
  (vec (remove #(= (first %) code) lenses)))

(defn instructions
  [data]
  (let [ins (map #(re-matches #"(\w+)([-=])(\d*)" %) (s/split data #"[,\n]"))]
    (loop [boxes (vec (repeat 256 []))
           [[_ code op num :as ins] & tail] ins]
      (if ins
        (let [hash (hash-string code)
              num (u/coerce-numeric-hard num)]
          #_ (prn :hey code hash op num)
          (case op
            "-" (recur (update boxes hash delete-lens code num)
                      tail)
            "=" (recur (update boxes hash replace-or-add-lens code num)
                      tail)
            ))
        boxes))))

(def to "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn score
  [boxes]
  (reduce +
          (flatten
           (map-indexed
            (fn [box-index lenses]
              (map-indexed
               (fn [lens-index lens]
                 (* (+ box-index 1)
                    (+ lens-index 1)
                    (second lens)))
               lenses))
            boxes))))

(score (instructions (data)))
