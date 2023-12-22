(ns aoc2023.day14
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [clojure.math.numeric-tower :as mnt]
            )
  )

(def data
  (->> "data/day14.txt"
       ju/file-lines
       ))

(defn score-col
  [data col]
  (loop [lines data
         counter 0
         stop (count data)]
    (let [line (first lines)
          lines-south (count lines)
          rest-lines (rest lines)]
      #_ (prn :x lines-south stop counter (nth line col))
      (if line
        (case (nth line col)
          \O (recur rest-lines (+ counter stop) (- stop 1))
          \. (recur rest-lines counter stop)
          \# (recur rest-lines counter (- lines-south 1)))
        counter))))

(defn part1
  [data]
  (reduce + (map (partial score-col data) (range (count (first data))))))

(def t0
  ["OOOO.#.O.."
   "OO..#....#"
   "OO..O##..O"
   "O..#.OO..."
   "........#."
   "..#....#.#"
   "..O..#.O.O"
   "..O......."
   "#....###.."
   "#....#...."])

(def t1
  ["..O.."
   ".O.O."
   ".O..O"
   ".#.O."
   "#.O.O"])

;;; Part 2

(defn adata
  [data]
  (mapv vec data))

(def a1 (adata t1))

;; Performance is going to suck, maybe use Java arrays

(defn rget
  [data x y]
  (get-in data [y x]))

(defn slide-row
  [r x y dx xs]
  (let [stop (loop [xx (+ x dx)]
               (if (<= 0 xx xs)
                 (case (rget r xx y)
                   \. (recur (+ xx dx))
                   \# xx
                   \O xx)
                 xx))]
    (prn :fuck x y stop (- stop dx) )
    (if (= x (- stop dx))
      r
      (-> r
        (assoc-in [y (- stop dx)] \O)
        (assoc-in [y x] \.)))))

(defn slide
  [data dx dy]
  ;; x dir
  (let [xc (count (first data))
        yc (count data)
        xs (if (pos? dx) xc 0)
        xe (if (pos? dx) 0 xc)
        ]
    (loop [r data
           y 0]
      (if (= y yc)
        r
        (recur
         (loop [r r
                x xs]
           (prn :yo x xs xe)
           (if (= x xe)
             r
             (case (rget r x y)
               \. (recur r (- x dx))
               \# (recur r (- x dx))
               \O (recur (slide-row r x y dx xs) (+ x dx) ))))
         (inc y))))))



      
      
         
         
    
