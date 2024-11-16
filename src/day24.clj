(ns day24
  (:require
   [clojure.string :as str]))

(def example
  "19, 13, 30 @ -2,  1, -2
   18, 19, 22 @ -1, -1, -2
   20, 25, 34 @ -2, -2, -4
   12, 31, 28 @ -1, -2, -1
   20, 19, 15 @  1, -5, -3")

(def input (slurp "24.txt"))

(defn parse-stone [line]
  (->> line
       (re-matches
        #"(-?\d+),\s+(-?\d+),\s+(-?\d+) @\s+(-?\d+),\s+(-?\d+),\s+(-?\d+)")
       rest
       (map parse-long)
       (partition 3)))

(defn parse [i]
  (->> (str/split-lines i)
       (map str/trim)
       (map parse-stone)))

(defn a+b [[[x y] [vx vy]]]
  (when (not= 0 vy)
    (let [a (/ vy vx)
          b (- y (* a x))]
      [a b])))

(defn- intersect [line1 line2]
  (let [[a c] (a+b line1)
        [b d] (a+b line2)]
    (when (and a b (not= b a))
      (let [x (/ (-  c d)
                 (- b a))
            y (+ (* a x) c)]
        (when (and (= (pos? (- x (ffirst line1)))
                      (pos? (first (second line1))))
                   (= (pos? (- x (ffirst line2)))
                      (pos? (first (second line2)))))
          [(double x) (double y)])))))

(defn- intersect? [mn mx line1 line2]
  (let [[x y] (intersect line1 line2)]
    (when (and x y)
      (and (<= mn x mx)
           (<= mn y mx)))))

(defn- pick-2 [xs]
  (let [xs (vec xs)
        n  (count xs)]
    (for [i     (range n)
          j     (range i n)
          :when (not= i j)]
      [(xs i) (xs j)])))

(defn- part1 [mn mx i]
  (let [lines (parse i)
        pairs (pick-2 lines)]
    (count (filter #(apply intersect? mn mx %) pairs))))

(defn- part2 []
  ;; need to determine 6 parameters
  ;; so just consider the first 6?
  ;; in the example there are only 5...
  ;; i guess that determines x in terms of the others,
  ;; then makes sure its on the right side
  )

(comment

  (pick-2 (range 3))

  (intersect [[19 13 30] [-2 1 -2]]
             [[18 19 22] [-1 -1 -2]])

  (part1 7 27 example)

  (part1 200000000000000 400000000000000 input))
