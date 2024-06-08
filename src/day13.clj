(ns day13
  (:require
    [clojure.string :as str]))

(defonce example
  "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defonce input (slurp "13.txt"))

;; Split the input into a sequence of grids
(defn chunk-grids [s]
  (->> (str/split-lines s)
       (partition-by empty?)
       (remove (comp empty? first))
       (map vec)))

(defn match-prefix? [xs ys]
  (every? true? (map = xs ys)))

;; Find the first column where the left and right halves of the grid are reflections
(defn reflection-col [grid]
  (->> (range 1 (count (first grid)))
       (filter (fn [col]
                 (->> grid
                      (every? (fn [row]
                                (let [left  (reverse (take col row))
                                      right (drop col row)]
                                  (match-prefix? left right)))))))
       first))

;; Find the first row where the top and bottom halves of the grid are reflections
(defn reflection-row [grid]
  (->> (range 1 (count grid))
       (filter (fn [row]
                 (let [top    (reverse (take row grid))
                       bottom (drop row grid)]
                   (->> (map match-prefix? top bottom)
                        (every? true?)))))
       first))

;; Score a grid based on the position of the row and/or column reflections
(defn score-row [[row col]]
  (cond-> 0
          row (+ row)
          col (+ (* 100 col))))

(defn part-1 [s]
  (->> (chunk-grids s)
       (map (juxt reflection-col reflection-row))
       (map score-row)
       (reduce +)))

(defn count-differences [xs ys]
  (->> (map not= xs ys)
       (filter true?)
       (count)))

(defn reflection-col' [grid]
  (->> (range 1 (count (first grid)))
       (map (fn [col]
              [col (->> grid
                        (map (fn [row]
                               (let [left  (reverse (take col row))
                                     right (drop col row)]
                                 (count-differences left right))))
                        (reduce +))]))
       (filter (comp #{1} second))
       ffirst))

(defn reflection-row' [grid]
  (->> (range 1 (count grid))
       (map (fn [row]
              [row (let [top    (reverse (take row grid))
                         bottom (drop row grid)]
                     (->> (map count-differences top bottom)
                          (reduce +)))]))
       (filter (comp #{1} second))
       ffirst))

(defn part-2 [s]
  (->> (chunk-grids s)
       (map (juxt reflection-col' reflection-row'))
       (map score-row)
       (reduce +)))

(comment
  (part-1 example)
  (part-1 input)

  (part-2 example)
  (part-2 input))
