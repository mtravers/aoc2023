(ns aoc2023.autils
  )

;;; Convert vec of strings to vec of vecs
(defn adata
  [data]
  (mapv vec data))

;;; Print vec of vecs
(defn pa
  [data]
  (doseq [l data]
    (println (apply str l))))

(defn rget
  [data x y]
  (get-in data [y x]))

(defn rset
  [data x y value]
  (assoc-in data [y x] value))

(def t0
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

(defn adims
  [data]
  [(count (first data)) (count data)])

(defn in-bounds
  [data x y]
  (let [[xs ys] (adims data)]
    (and (< -1 x xs) (< -1 y ys))))

#_
(pa (rset (adata t0) 1 1 \*))

;;; Bounds checking
