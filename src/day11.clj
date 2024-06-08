(ns day11
  (:require [utils :as u]))

(def example
  "...#......
   .......#..
   #.........
   ..........
   ......#...
   .#........
   .........#
   ..........
   .......#..
   #...#.....")

(defn dist-manhattan
  ([[x1 x2]]
   (dist-manhattan x1 x2))
  ([[i1 j1] [i2 j2]]
   (+ (abs (- i1 i2))
      (abs (- j1 j2)))))

(defn pairs-upper [n]
  (for [i (range n)
        j (range (inc i) n)]
    [i j]))

(defn galaxy-at [galaxies [i j]]
  [(get galaxies i) (get galaxies j)])

(defn galaxy-pairs [galaxies]
  (map (partial galaxy-at galaxies)
       (pairs-upper (count galaxies))))

(defn transpose [grid]
  (for [j (range (count (first grid)))]
    (apply str (for [i (range (count grid))]
                 (u/get-2d grid i j)))))

(defn blank-row? [row]
  (every? #{\.} row))

(defn expand-universe-vertically [grid]
  (vec (mapcat #(if (blank-row? %) [% %] [%]) grid)))

(defn expand-universe [grid]
  (-> grid
      expand-universe-vertically
      transpose
      expand-universe-vertically
      transpose))

(defn find-galaxies [grid]
  (vec
    (for [i (range (count grid))
          j (range (count (first grid)))
          :when (= \# (u/get-2d grid i j))]
      [i j])))

(defonce input (slurp "11.txt"))

(defn part-1 [i]
  (->> (u/lines i)
       (expand-universe)
       (find-galaxies)
       (galaxy-pairs)
       (map dist-manhattan)
       (u/sum)))

(defn blank-rows [grid]
  (keep-indexed
    (fn [i row]
      (when (blank-row? row)
        i))
    grid))

(defn galaxy-distance [time blank-row? blank-col? [[i1 j1] [i2 j2]]]
  ;; make sure that i1 <= i2 and j1 <= j2
  ;; we already have i1 <= i2 because we scan the grid for galaxies in row order
  (let [[j1 j2] (sort [j1 j2])
        dis (range 1 (inc (- i2 i1)))
        djs (range 1 (inc (- j2 j1)))
        i-dist (u/sum (map #(if (blank-row? (+ i1 %)) time 1) dis))
        j-dist (u/sum (map #(if (blank-col? (+ j1 %)) time 1) djs))]
    (+ i-dist j-dist)))


(defn part-2
  ([i]
   (part-2 1000000 i))
  ([time i]
   (let [grid (u/lines i)
         galaxies (find-galaxies grid)
         blank-row? (set (blank-rows grid))
         blank-col? (set (blank-rows (transpose grid)))
         pos-pairs (galaxy-pairs galaxies)]

     #_(clojure.pprint/pprint
       (into (sorted-map)
             (zipmap
               (map #(mapv inc %) (pairs-upper (count galaxies)))
               (map (partial galaxy-distance time blank-row? blank-col?) pos-pairs))))

     (u/sum (map (partial galaxy-distance time blank-row? blank-col?) pos-pairs)))))

(comment
  (part-1 example)
  (part-1 input)

  (part-2 2 example)
  (part-2 10 example)
  (part-2 100 example)
  ;; 82000210 is too low
  (part-2 example)

  (part-2 input))
