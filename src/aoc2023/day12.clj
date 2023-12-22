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

;;; Exhaustive search
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


;;; Part 2

(defn some=
  [v seq]
  (some #(= v %) seq))

;;; Memoizing this didn't help much
(defn maybe-passes?
  [sv counts]
  (let [springs (mapv #(if (= (first %) \.) (rest %) %)
                      (remove #(= % '(\.))
                              (u/partition-if #(= \. %) sv)))]
    (loop [[spring & rest-springs] springs
           [gcount & rest-counts] counts]
      (if (or (nil? spring)
              (some= \? spring))
        true
        (if (= (count spring) gcount)
          (recur rest-springs
                 rest-counts)
          false)))))

(defn count-ways-2-dumb
  [original seqs]
  (count-ways (s/join "?" (repeat 5 original))
              (vec (apply concat (repeat 5 seqs)))))


(defn count-ways-2
  ([original seqs sv i]
   #_ (prn :y sv i)
   (let [first? (s/index-of original \? i)]
     (if first?
       (if (maybe-passes? sv seqs)
         (+ (count-ways-2 original seqs (assoc sv first? \#) (inc first?))
            (count-ways-2 original seqs (assoc sv first? \.) (inc first?)))
         0)
       (if (passes? sv seqs) 1 0))))
  ([original seqs]
   (prn :x original seqs)
   (let [expanded (s/join "?" (repeat 5 original))]
     (count-ways-2 expanded
                   (vec (apply concat (repeat 5 seqs)))
                   (vec expanded) 0))))

(defn part2
  []
  (reduce + (map #(count-ways-2 (first %) (second %)) data)))

;;; Part 2, faster (nope, no speed improvement)

(defmacro memoize-rec [form]
  (let [[fn* fname params & body] form
        params-with-fname (vec (cons fname params))]
    `(let [f# (memoize (fn ~params-with-fname
                         (let [~fname (partial ~fname ~fname)] ~@body)))]
       (partial f# f#))))

(defn count-ways-2
  ([original seqs sv i]
   (let [recurse0 (memoize-rec
                  (fn recurse [sv i]
                    (let [first? (s/index-of original \? i)]
                      (if first?
                        (if (maybe-passes? sv seqs)
                          (+ (recurse (assoc sv first? \#) (inc first?))
                             (recurse (assoc sv first? \.) (inc first?)))
                          0)
                        (if (passes? sv seqs) 1 0)))))]
     (recurse0 sv i)
     ))
  ([original seqs]
   (prn :x original seqs)
   (let [expanded (s/join "?" (repeat 5 original))]
     (count-ways-2 expanded
                   (vec (apply concat (repeat 5 seqs)))
                   (vec expanded) 0))))
