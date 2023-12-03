(ns aoc2023.p2
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

(def items (ju/file-lines "data/day2.txt"))

(defn possible?
  [game]
  (every? #(and (<= (get % "red" 0) 12)
                (<= (get % "green" 0) 13)
                (<= (get % "blue" 0) 14))
          game))

(defn process-item
  [item]
  (let [[_ game list] (re-matches #"Game (\d+): (.*?(;|$))" item)
        draws (s/split list #"; ")
        parsed
        (for [draw draws]
          (into {}
                (for [item (s/split draw #", ")]
                  (let [[_ n color] (re-matches #"(\d+) (.*)" item)]
                    [color (u/coerce-numeric n)]))))]
    (if (possible? parsed)
      (u/coerce-numeric game)
      0)))

(reduce + (map process-item items))

;;; Part 2

(defn process-item-2
  [item]
  (let [[_ game list] (re-matches #"Game (\d+): (.*?(;|$))" item)
        draws (s/split list #"; ")
        parsed
        (for [draw draws]
          (into {}
                (for [item (s/split draw #", ")]
                  (let [[_ n color] (re-matches #"(\d+) (.*)" item)]
                    [color (u/coerce-numeric n)]))))]
    (let [blue (apply max (map #(get % "blue" 0) parsed))
          green (apply max (map #(get % "green" 0) parsed))
          red (apply max (map #(get % "red" 0) parsed))]
      (* blue green red))))
          
(reduce + (map process-item-2 items))
