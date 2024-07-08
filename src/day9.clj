(ns day9
  (:require
   [utils :as u]))

(def example
  "0 3 6 9 12 15
   1 3 6 10 15 21
   10 13 16 21 30 45")

(def input (slurp "9.txt"))

(defn parse-line [line]
  (map parse-long (u/split-ws line)))

(defn next-digit [row]
  (loop [row row
         last-digits (list (last row))]
    (let [differences (map - (rest row) row)]
      (if (every? zero? differences)
        (reduce + last-digits)
        (recur differences (cons (last differences) last-digits))))))

(defn part-1 [i]
  (->> (u/lines i)
       (map parse-line)
       (map next-digit)
       (u/sum)))

(defn next-digit-2 [row]
  (loop [row row
         initial-digits (list (first row))]
    (let [differences (map - (rest row) row)]
      (if (every? zero? differences)
        (reduce (u/flip -) 0 initial-digits)
        (recur differences (cons (first differences) initial-digits))))))

(comment
  (next-digit-2 [0 0 0])
  (next-digit-2 [0 1 0])
  (next-digit-2 [0 1 1])
  (next-digit-2 [0 3 6 9 12 15])
  (next-digit-2 [1 3 6 10 15 21])
  (next-digit-2 [10 13 16 21 30 45]))

(defn part-2 [i]
  (->> (u/lines i)
       (map parse-line)
       (map next-digit-2)
       (u/sum)))

(comment
  (part-1 example)
  (part-1 input)

  (part-2 example)
  (part-2 input))
