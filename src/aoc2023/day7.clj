(ns aoc2023.day7
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

;;; Data
(def data
  (-> "data/day7.txt"
      ju/file-lines
      (u/pam #(let [[cards v] (s/split % #" ")]
                [cards (u/coerce-numeric v)]))))

(def cards "AKQJT98765432")

(defn card-rank
  [c]
  (- (s/index-of cards c)))

(defn rank-hand
  [h]
  (let [freqs (frequencies h)
        with (fn [n] (u/some-thing (fn [[c f]] (= f n)) freqs))
        type
        (cond (with 5) 0
              (with 4) -1
              (and (with 3) (with 2)) -2
              (with 3) -3
              (some #(= 2 %) (vals (frequencies (vals freqs)))) -4
              (with 2) -5
              :else -6)]
    (apply vector type (map card-rank h) )))

(def test
  [["32T3K" 765]
  ["T55J5" 684]
  ["KK677" 28]
  ["KTJJT" 220]
  ["QQQJA" 483]]
  )

(defn rank-hands
  [hands]
  (let [hands (map #(conj % (rank-hand (first %))) hands)]
    (sort-by #(nth % 2) hands)))

(defn score-hands
  [hands]
  (reduce +
          (map-indexed (fn [i [_ score _]]
                         (* score (+ i 1)))
                       (rank-hands hands))))
  
