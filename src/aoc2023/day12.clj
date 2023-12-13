(ns aoc2023.day12
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [clojure.math.numeric-tower :as mnt]
            )
  )

(def data
  (map (fn [l]
         (let [[s seqs] (s/split l #" ")]
           [s (mapv u/coerce-numeric (s/split seqs #","))]))
       (ju/file-lines "data/day12.txt")))

(def tests
  [["???.###" [1 1 3] 1]
   [".??..??...?##." [1 1 3] 4]
   ["?#?#?#?#?#?#?#?" [1 3 1 6] 1]
   ["????.#...#..." [4 1 1] 1]
   ["????.######..#####." [1 6 5] 4]
   ["?###????????" [3 2 1] 10]])

(defn passes?
  [sv seqs]
  (= seqs
     (mapv #(if (= (first %) \.) (count (rest %)) (count %))
           (remove #(= % '(\.))
                   (u/partition-if #(= \. %) sv)))))

;;; Exhaustive search, works on tests but not on real data
(defn count-ways
  ([original seqs sv i]
   (let [first? (s/index-of original \? i)]
     (if first?
       (+ (count-ways original seqs (assoc sv first? \#) (inc first?))
          (count-ways original seqs (assoc sv first? \.) (inc first?)))
       (if (passes? sv seqs) 1 0))))
  ([original seqs]
   #_ (prn :x original seqs)
   (count-ways original seqs (vec original) 0)))
  
(defn check-tests
  []
  (for [[s seqs count :as test] tests]
    (if (= count (count-ways s seqs))
      (prn :good test)
      (prn :bad test (count-ways s seqs)))))

(defn part1
  []
  (reduce + (map #(count-ways (first %) (second %)) data)))




