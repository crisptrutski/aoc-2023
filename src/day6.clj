(ns day6
  (:require
   [clojure.string :as str]))

(def example
  "Time:      7  15   30
   Distance:  9  40  200")

(def input (slurp "6.txt"))

(defn parse-time-distance-pairs [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (->> (str/split line #"[^\d]+")
                   (remove empty?)
                   (map parse-long))))
       (apply zipmap)))

(defn ways [dt dx]
  (->> (dec dt)
       (range 1)
       (map (fn [push] (* push (- dt push))))
       (filter (partial < dx))
       (count)))

(defn part-1 [input]
  (->> (parse-time-distance-pairs input)
       (map #(apply ways %))
       (reduce *)))

(defn parse-total-time-distance [input]
  (mapv (fn [line]
          (parse-long (str/replace line #"[^\d]" "")))
        (str/split-lines input)))

(defn ways-optimized [dt dx]
  ;; use symmetry, and short-circuit search starting from greatest distance
  (let [half-time (/ dt 2)]
    (reduce
     (fn [acc i]
       (if (> (* i (- dt i)) dx)
         (+ acc (if (= i half-time) 1 2))
         (reduced acc)))
     0
     (range (long (Math/floor half-time)) 1 -1))))

(defn part-2-naive [input]
  (->> (parse-total-time-distance input)
       (apply ways)))

(defn part-2 [input]
  (->> (parse-total-time-distance input)
       (apply ways-optimized)))

;; ---------

(comment
  (time (part-1 example))
  (time (part-1 input))

  (time (part-2-naive example))
  (time (part-2-naive input))

  (time (part-2 example))
  (time (part-2 input)))
