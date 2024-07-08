(ns day4
  (:require
   [clojure.string :as str]))

(def example
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def input (slurp "4.txt"))

(defn strip-start [s] (subs s (inc (str/index-of s ":"))))

(defn parse-nums [s]
  (map parse-long (str/split (str/trim s) #"\s+")))

(defn parse-line [s]
  (map parse-nums (str/split (strip-start s) #"\|")))

(parse-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(defn count-matches [winning actual]
  (count (keep (set winning) actual)))

(defn score-line [[winning actual]]
  (let [c (count-matches winning actual)]
    (if (pos? c) (long (Math/pow 2 (dec c))) 0)))

(defn part-1 [input]
  (->> input
       (str/split-lines)
       (map parse-line)
       (map score-line)
       (reduce +)))

(seq '())

(defn map-all [f xs ys]
  (if (or (seq xs) (seq ys))
    (let [[x & xs] xs
          [y & ys] ys]
      (lazy-seq
       (cons (f x y)
             (map-all f xs ys))))))

(defn calc-round [[total-count upcoming-counts] next-card]
  (let [[winning actual] next-card
        [this-copies & following-counts] (or upcoming-counts [0])
        this-count (inc this-copies)
        additional-cards (count (keep (set winning) actual))
        additional-counts (repeat additional-cards this-count)
        total-count (+ total-count this-count)
        upcoming-counts (map-all (fnil + 0 0)
                                 following-counts
                                 additional-counts)]
    [total-count upcoming-counts]))

(defn part-2 [input]
  (->> input
       (str/split-lines)
       (map parse-line)
       (reduce calc-round [0 nil])
       (first)))

;; ----------

(part-1 example)
(part-1 input)

(part-2 example)
(part-2 input)
