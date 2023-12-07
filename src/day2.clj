(ns day2
  (:require [clojure.string :as str]))

(def example
  (str
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"))

(def input (slurp "2.txt"))

(def game-num-pattern #"Game (\d+)")

(def count-pattern #"((\d+) (red|green|blue))")

(defn game-count [s] (Integer/parseInt (second (re-find game-num-pattern s))))

(defn parse-play [s]
  (zipmap
    (map second s)
    (map #(Integer/parseInt (first %)) s)))

(defn max-count-valid? [max-counts]
  (and (<= (get max-counts "red" 0) 12)
       (<= (get max-counts "green" 0) 13)
       (<= (get max-counts "blue" 0) 14)))

(defn required-count [line]
  (->> (str/split (str/replace-first line #".*: " "") #";")
       (map (comp parse-play #(map (partial drop 2) %) (partial re-seq count-pattern)))
       (apply merge-with max)))

(defn game-valid? [line]
  (max-count-valid? (required-count line)))

(defn part-1 [input]
  (reduce
    +
    (map
      (fn [idx valid?] (if valid? idx 0))
      (map game-count (str/split-lines input))
      (map game-valid? (str/split-lines input)))))


(defn part-2 [input]
  (->> (str/split-lines input)
       (map required-count)
       (map #(map second %))
       (map #(reduce * %))
       (reduce +)))

(part-1 example)
(part-1 input)

(part-2 example)
(part-2 input)
