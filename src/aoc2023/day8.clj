(ns aoc2023.day8
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [clojure.math.numeric-tower :as mnt]
            )
  )

;;; Data
(defn process-data
  [lines]
  (let [[moves _ & nodes] lines]
    {:moves moves
     :nodes (u/index-by second (map (partial re-matches #"(\w+) = \((\w+), (\w+)\)") nodes))}))


(def test-source
  ["LR"
  ""
  "11A = (11B, XXX)"
  "11B = (XXX, 11Z)"
  "11Z = (11B, XXX)"
  "22A = (22B, XXX)"
  "22B = (22C, 22C)"
  "22C = (22Z, 22Z)"
  "22Z = (22B, 22B)"
  "XXX = (XXX, XXX)"])


(def data (process-data (ju/file-lines "data/day8.txt")))
(def test (process-data test-source))
    

(defn part1
  [{:keys [moves nodes]}]
  (loop [current "AAA"
         i 0
         counter 0]
    (if (= current "ZZZ")
      counter
      (recur (case (get moves i)
               \L (get-in nodes [current 2])
               \R (get-in nodes [current 3]))
             (mod (inc i) (count moves))
             (inc counter)))))

;;; Doesn't work. Blows out stack, which is weird, but in any case this is too brute-force
(defn part2
  [{:keys [moves nodes]}]
  (loop [current (filter #(= \A (nth % 2)) (keys nodes))
         i 0
         counter 0]
    (if (every? #(= \Z (nth % 2)) current)
      counter
      (recur (map #(case (get moves i)
                     \L (get-in nodes [% 2])
                     \R (get-in nodes [% 3]))
                  current)
             (mod (inc i) (count moves))
             (inc counter)))))

(defn part2a
  [{:keys [moves nodes]}]
  (let [seeds (filter #(= \A (nth % 2)) (keys nodes))
        counts (map (fn [seed]
                       (loop [current seed
                              i 0
                              counter 0]
                         (if (= \Z (nth current 2))
                           counter
                           (recur (case (get moves i)
                                    \L (get-in nodes [current 2])
                                    \R (get-in nodes [current 3]))
                                  (mod (inc i) (count moves))
                                  (inc counter)))))
                    seeds)]
    (reduce mnt/lcm counts)))
    
    
