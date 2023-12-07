(ns day1
  (:require [clojure.string :as str]
            [utils :as u]))

(def example
  "1abc2
   pqr3stu8vwx
   a1b2c3d4e5f
   treb7uchet")

(def input (slurp "1.txt"))

(defn parse-line [s]
  (parse-long
    (str
      (second (re-find #"(\d)" s))
      (second (re-find #".*(\d)" s)))))

(defn part-1 [input]
  (u/sum (map parse-line (u/lines input))))

;; ---

(def example-2
  "two1nine
   eightwothree
   abcone2threexyz
   xtwone3four
   4nineeightseven2
   zoneight234
   7pqrstsixteen")

(def digits
  ["zero"
   "one"
   "two"
   "three"
   "four"
   "five"
   "six"
   "seven"
   "eight"
   "nine"])

(def digit->long (zipmap digits (range)))

(defn parse-digit [s]
  (get digit->long s s))

(defn parse-digit-matching [regex s]
  (parse-digit (second (re-find regex s))))

(def pattern (str "(" (str/join "|" digits) "|\\d)"))

(defn parse-line-2 [l]
  (parse-long
    (str
      (parse-digit-matching (re-pattern pattern) l)
      (parse-digit-matching (re-pattern (str ".*" pattern)) l))))

(defn part-2 [input]
  (->> (u/lines input)
       (map parse-line-2)
       (reduce +)))

(println
  (part-1 example)
  (part-1 input))

(println
  (part-2 example-2)
  (part-2 input))