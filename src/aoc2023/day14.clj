(ns aoc2023.day14
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [clojure.math.numeric-tower :as mnt]
            [nextjournal.clerk :as clerk]
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

(def t2
  ["O...."
   "....O"
   "..O.."
   "O.O#."
   "O.#.."
   ".O.O#"
   ".#.O."
   "O.O.#"])

;;; Part 2

(defn adata
  [data]
  (mapv vec data))

(def a1 (adata t1))

(defn pa
  [data]
  (doseq [l data]
    (println (apply str l))))

;; Performance is going to suck, maybe use Java arrays

(defn rget
  [data x y]
  (get-in data [y x]))

(defn slide-row
  [r x y dx xc]
  #_ (prn :slide-row x y dx xc)
  (let [stop (loop [xx (+ x dx)]
               (if (contains? (first r) xx)
                 (case (rget r xx y)
                   \. (recur (+ xx dx))
                   \# xx
                   \O xx)
                 (if (pos? dx) xc -1)))]
    (if (= x (- stop dx))
      r
      (-> r
        (assoc-in [y (- stop dx)] \O)
        (assoc-in [y x] \.)))))

(defn slide-x
  [data dx]
  ;; x dir
  (let [xc (count (first data))
        yc (count data)
        [xs xe] (if (pos? dx) [(dec xc) -1] [0 xc])
        ]
    (loop [r data
           y 0]
      (if (= y yc)
        r
        (recur
         (loop [r r
                x xs]
           #_ (prn :x x)
           (if (= x xe)
             r
             (case (rget r x y)
               \. (recur r (- x dx))
               \# (recur r (- x dx))
               \O (recur (slide-row r x y dx xc) (- x dx) ))))
         (inc y))))))
    
(def sl1 (slide-x a1 -1))

;;; This for the y direction, copying the x code which is inelegant but I'm lazy

(defn slide-col
  [r x y dy yc]
  (let [stop (loop [yy (+ y dy)]
               (if (contains? r yy)
                 (case (rget r x yy)
                   \. (recur (+ yy dy))
                   \# yy
                   \O yy)
                 (if (pos? dy) yc -1)))]
    #_ (prn :slide-col x y stop)
    (if (= y (- stop dy))
      r
      (-> r
        (assoc-in [(- stop dy) x] \O)
        (assoc-in [y x] \.)))))

(defn slide-y
  [data dy]
  ;; y dir
  (let [xc (count (first data))
        yc (count data)
        [ys ye] (if (pos? dy) [(dec yc) -1] [0 yc])
        ]
    (loop [r data
           x 0]
      (if (= x xc)
        r
        (recur
         (loop [r r
                y ys]
           #_ (prn :y y)
           (if (= y ye)
             r
             (case (rget r x y)
               \. (recur r (- y dy))
               \# (recur r (- y dy))
               \O (recur (slide-col r x y dy yc) (- y dy) ))))
         (inc x))))))

(defn fcycle
  [r]
  (-> r
      (slide-y -1)
      (slide-x -1)
      (slide-y 1)
      (slide-x 1)))

(defn exhausting
  [data]
  (loop [i 0
         ht {}
         state (adata data)]
    (let [new-state (fcycle state)]
      (if (contains? ht new-state)
        (do (prn :cycle :yay)
            [i (get ht new-state)])
        (recur (inc i)
               (assoc ht state [new-state i])
               new-state)))))

(defn state-after
  [data cycles]
  (let [[psycle-end [psycle-start-data psycle-start]] (exhausting data)
        modpos (inc (rem (- cycles psycle-start) (- psycle-end psycle-start)))]
    (prn :hey psycle-start psycle-end modpos)
    (nth (iterate fcycle psycle-start-data) modpos)))

(defn load
  [data]
  (reduce
   +
   (for [x (range (count (first data)))
         y (range (count data))]
     (if (= \O (rget data x y))
       (- (count data) y)
       0))))

;;; Clerking it

#_
(clerk/serve! {:browse? true :port 6611
               :watch-paths ["aoc2023" "src"]})

(defn termwalk
  [f struct]
  (clojure.walk/postwalk
   #(if (coll? %) % (f %)) struct))

#_
(clerk/table (termwalk str a1))


(clerk/html
 [:table {:style {:width "initial"}}
  (for [row a1]
    [:tr
     (for [col row]
       [:td {:style {:border "1px solid  lightgray" :text-align "center" :padding "3px"}}
        (str col)])])])

(defn cd
  [data]
  (clerk/html
   [:table {:style {:width "initial"}}
    (for [row data]
      [:tr
       (for [col row]
         [:td {:style {:border "1px solid  lightgray" :text-align "center" :padding "3px"}}
          (str col)])])]))

(cd a1)

(clerk/row
 (cd a1)
 (cd (slide-x a1 -1))
 (cd (slide-x a1 1))
 (cd (slide-y a1 -1))
 (cd (slide-y a1 1)))
