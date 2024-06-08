(ns day14
  (:require [clojure.string :as str]))

(def example
  "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(def input (slurp "14.txt"))

(defn columns [grid]
  (apply map vector grid))

(defn rolled-positions [column]
  (first
    (reduce
      (fn [[sum next-dist] [pos char]]
        (case char
          \# [sum (dec pos)]
          \. [sum next-dist]
          \O [(+ sum next-dist) (dec next-dist)]))
      [0 (count column)]
      (map vector
           (range (count column) 0 -1)
           column))))

(defn rolled-load [columns]
  (reduce + (map rolled-positions columns)))

(defn part1 [input]
  (->> input str/split-lines columns rolled-load))

(defn rotate-right [grid]
  (->> grid columns reverse))

(defn roll-north* [column]
  (take (count column)
        (concat
          (first
            (reduce
              (fn [[acc spaces] char]
                (case char
                  \. [acc (inc spaces)]
                  \O [(conj acc \O) spaces]
                  \# [(into acc (concat (repeat spaces \.)
                                        [\#]))
                      0]))
              [[] 0]
              column))
          (repeat \.))))

(defn roll-north [columns]
  (map roll-north* columns))

(defn roll-cycle [columns]
  (reduce (fn [acc _]
            (-> acc roll-north rotate-right))
          columns
          (range 4)))


(defn t [x] (clojure.pprint/pprint x) x)

(defn p [columns]
  (println (str/join "\n" (apply map str columns)))
  (println "")
  columns)

(defn north-load [columns]
  (reduce + (map (fn [col]
                   ([[reduce]] (fn [s [c i]]
                             (if (= \O c)
                               (+ s i)
                               s))
                           0
                           (map vector
                                col
                                (range (count col) 0 -1))))
                 columns)))

(def cycles 1000000000)

(defn repeat-with-cycles [f x n]
  (loop [x          x
         n          n
         i          0
         first-seen {x 0}]
    (if (zero? n)
      x
      (let [after (roll-cycle x)]
        (if-let [j (first-seen after)]
          (let [cycle-length (- (inc i) j)
                net-cycles   (rem (dec n) cycle-length)]
            (repeat-with-cycles f after net-cycles))
          (recur after
                 (dec n)
                 (inc i)
                 (assoc first-seen after (inc i))))))))

(defn part2 [input]
  (let [cols (->> input str/split-lines columns)]
    (north-load (repeat-with-cycles roll-cycle cols cycles))))

(comment
  (-> example str/split-lines
      columns p
      roll-cycle p
      roll-cycle p))



(comment
  (part1 example)
  (part1 input)

  (part2 example)
  (part2 input))







