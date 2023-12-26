(ns aoc2023.day16
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [clojure.math.numeric-tower :as mnt]
            [nextjournal.clerk :as clerk]
            )
  (:use aoc2023.autils)
  )

(def t1
  [".|...\\...."
   "|.-.\\....."
   ".....|-..."
   "........|."
   ".........."
   ".........\\"
   "..../.\\\\.."
   ".-.-/..|.."
   ".|....-|.\\"
   "..//.|...."])

(def data
  (vec (ju/file-lines "data/day16.txt")))




(defn moves
  [data [x y dx dy :as state]]
  (filter
   #(in-bounds data (first %) (second %)) 
   (case (rget data x y)
     \. [[(+ x dx) (+ y dy) dx dy]]
     \| (if (= dx 0)
          [[x (+ y dy) dx dy]]
          [[x (- y 1) 0 -1]
           [x (+ y 1) 0 1]])
     \- (if (= dy 0)
          [[(+ x dx) y dx dy]]
          [[(- x 1) y -1 0]
           [(+ x 1) y 1 0]])
     \\ [(case [dx dy]
           [1 0] [x (inc y) 0 1]
           [-1 0] [x (dec y) 0 -1]
           [0 1] [(inc x) y 1 0]
           [0 -1] [(dec x) y -1 0]
           )]
     \/ [(case [dx dy]
           [1 0] [x (dec y) 0 -1]
           [-1 0] [x (inc y) 0 1]
           [0 1] [(dec x) y -1 0]
           [0 -1] [(inc x) y 1 0]
           )])))

(defn cd
  [data]
  (clerk/html
   [:table {:style {:width "initial"}}
    (for [row data]
      [:tr
       (for [col row]
         [:td {:style {:border "1px solid lightgray" :text-align "center"  :width "25px" :height "25px" :font-weight "bold"}}
          (str col)])])]))

;;; → multitool, should check for exactly one binding
(defmacro for-indexed
  [bindings index body]
  `(map-indexed (fn [~index ~(first bindings)] ~body)
                ~(second bindings)))
  

(defn cd-m
  [data fringe]
  (let [points (set (map #(vec (take 2 %)) fringe))]
    (clerk/html
     [:table {:style {:width "initial"}}
      (for-indexed [row data] ir
        [:tr
         (for-indexed [col row] ic
                      [:td {:style {:border "1px solid lightgray"
                                    :text-align "center"
                                    :width "25px" :height "25px"
                                    :background-color (if (contains? points [ic ir]) "yellow" "white")
                                    :font-weight "bold"}}
                       (str col)])])])))

(defn cd-n
  [data marked fringe]
  (let [points (set (map #(vec (take 2 %)) fringe))]
    (clerk/html
     [:table {:style {:width "initial"}}
      (for-indexed [row data] ir
        [:tr
         (for-indexed [col row] ic
                      [:td {:style {:border "1px solid lightgray"
                                    :text-align "center"
                                    :width "25px" :height "25px"
                                    :background-color (if (contains? points [ic ir])
                                                        "yellow"
                                                        (if (= \* (rget marked ic ir)) "pink" "white"))
                                    :font-weight "bold"}}
            (str col)])])])))

(defn acount
  [v data]
  (count (filter #(= v %) (flatten data))))

(defn iter
  [data]
  (loop [marked data
         fringe [[0 0 1 0]]
         visited #{}
         i 0]
    (if (empty? fringe)
      (acount \* marked)
      (let [marked (reduce (fn [array [x y]] (rset array x y \*)) marked fringe)
            moves (remove visited (mapcat (partial moves data) fringe))]
        #_ (tap> (clerk/row [i fringe] (cd-n data marked fringe)))
        (recur marked
               moves
               (set/union visited (set fringe))
               (inc i))))))
      
#_    
(clerk/serve! {:browse? true
               :port 6611
               :watch-paths ["src" "src/aoc2023"]})

(def a1 (adata t1))
#_
(iter a1)
