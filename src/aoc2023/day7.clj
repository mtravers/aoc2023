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
  

;;; Part 2

(def cards2 "AKQT98765432J")

(defn card-rank-2
  [c]
  (- (s/index-of cards2 c)))

;;; Multitool! But needs a better implementation
(defn bag=
  [seq1 seq2]
  (and (= (count seq1) (count seq2))
       (= (sort seq1) (sort seq2))))

;;; Full house cases AAABB JAABB
(defn full-house
  [freqs]
  (or (bag= (vals freqs) [3 2])
      (and (= 1 (get freqs \J))
           (bag= (vals freqs) [1 2 2]))))
  
;;; You can't have a two-pair hand with a J so this is simple
(defn two-pair
  [freqs]
  (bag= (vals freqs) [2 2 1]))

(defn rank-hand-2
  [h]
  (let [freqs (frequencies h)
        non-j-freqs (dissoc freqs \J)
        jokers (get freqs \J 0)
        with (fn [n] (u/some-thing (fn [[c f]] (= f n)) non-j-freqs))
        with-jokers (fn [n] (with (- n jokers)))
        type
        (cond (or (with-jokers 5) (= h "JJJJJ")) 0
              (with-jokers 4) -1
              (full-house freqs) -2 
              (with-jokers 3) -3
              (two-pair freqs) -4
              (with-jokers 2) -5
              :else -6)]
    (apply vector type (map card-rank-2 h) )))

;;; Copying rather than parameterizing because lazy
(defn rank-hands-2
  [hands]
  (let [hands (map #(conj % (rank-hand-2 (first %))) hands)]
    (sort-by #(nth % 2) hands)))

(defn score-hands-2
  [hands]
  (reduce +
          (map-indexed (fn [i [_ score _]]
                         (* score (+ i 1)))
                       (rank-hands-2 hands))))


