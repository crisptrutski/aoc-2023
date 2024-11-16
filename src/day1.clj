(ns day1
  (:require
   [clojure.string :as str]
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
    (u/capture #"(\d)" s)
    (u/capture #".*(\d)" s))))

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

;; Converting to a map is more efficient that searching for index
(def digit->long (zipmap digits (range)))

;; Overloaded for reading literal or named versions
(defn parse-digit [s]
  (get digit->long s s))

(def pattern
  (re-pattern (str (str/join "|" digits) "|\\d")))

(defn parse-line-2 [line]
  (let [digits (map parse-digit (re-seq pattern line))]
    (parse-long
     (str (first digits)
          (last digits)))))

(defn part-2 [input]
  (transduce (map parse-line-2) + (u/lines input)))

(comment
  (println
   (part-1 example)
   (part-1 input))

  (println
   (part-2 example-2)
   (part-2 input)))
