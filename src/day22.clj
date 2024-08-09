(ns day22
  (:require
   [clojure.string :as str]
   [utils :as u]))

(def example
  "1,0,1~1,2,1
   0,0,2~2,0,2
   0,2,3~2,2,3
   0,0,4~0,2,4
   2,0,5~2,2,5
   0,1,6~2,1,6
   1,1,8~1,1,9")

(def input (slurp "22.txt"))

(defn- dir [x y]
  (if (>= y x)
    1
    -1))

(defn range-incl [a b]
  (conj (range a b (dir a b)) a b))

(defn- fill-bricks [[[x1 y1 z1 :as p1] [x2 y2 z2 :as p2]]]
  (into (sorted-set)
   (for [x (range-incl x1 x2)
         y (range-incl y1 y2)
         z (range-incl z1 z2)]
     [x y z])))

(defn- parse-brick [l]
  (fill-bricks
   (mapv (comp #(map parse-long %)
              #(str/split % #",")
              str/trim)
        (str/split l #"~"))))


(defn parse [s]
  (mapv parse-brick (str/split-lines s)))

(prn (parse example))

(defn part-1 [s]
  )

(defn part-2 [s]
  )

(comment
  (part-1 example)
  (part-1 input)

  (part-2 example)
  (part-2 input))
