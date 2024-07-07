(ns day8
  (:require
   [clojure.string :as str]
   [utils :as u]))

(def example-1
  "RL

   AAA = (BBB, CCC)
   BBB = (DDD, EEE)
   CCC = (ZZZ, GGG)
   DDD = (DDD, DDD)
   EEE = (EEE, EEE)
   GGG = (GGG, GGG)
   ZZZ = (ZZZ, ZZZ)")

(def example-2
  "LLR

   AAA = (BBB, BBB)
   BBB = (AAA, ZZZ)
   ZZZ = (ZZZ, ZZZ)")

(def input (slurp "8.txt"))

(def pos-pattern "([0-9A-Z]{3})")

(def pos-patterns
  (re-pattern (str/join ".*" (repeat 3 pos-pattern))))

(defn parse-row [row]
  (let [[_ from left right] (re-find pos-patterns row)]
    (if (and from left right)
      [from [left right]])))

(defn step [adjacency dir pos]
  (let [[left right] (get adjacency pos)]
    (assert (and left right))
    (case dir
      \L left
      \R right
      :else (throw (ex-info (str "Unexpected direction: " dir) {:dir dir})))))

(defn parse [i]
  (let [[dirs & body] (remove empty? (u/lines i))
        adjacency (into {} (keep parse-row body))]
    [dirs adjacency]))

(defn steps-until [adjacency dirs pred start]
  (first (reduce
          (fn [[len pos] dir]
            (let [next-len (inc len)
                  next-pos (step adjacency dir pos)
                  next [next-len next-pos]]
              (if (pred next-pos)
                (reduced next)
                next)))
          [0 start]
          (cycle dirs))))

(defn part-1 [i]
  (let [[dirs adjacency] (parse i)]
    (steps-until adjacency dirs #{"ZZZ"} "AAA")))

(def example-3
  "LR

   11A = (11B, XXX)
   11B = (XXX, 11Z)
   11Z = (11B, XXX)
   22A = (22B, XXX)
   22B = (22C, 22C)
   22C = (22Z, 22Z)
   22Z = (22B, 22B)
   XXX = (XXX, XXX)")

(defn part-2 [i]
  (let [[dirs adjacency] (parse i)
        starts (filter (u/ending-with "A") (keys adjacency))
        terminal? (u/ending-with "Z")
        ghost-length (partial steps-until adjacency dirs terminal?)
        lengths (map ghost-length starts)]
    (reduce u/lcm lengths)))


(comment
  (u/printing
   (part-1 example-1)
   (part-1 example-2)
   (part-1 input))

  (println "------")

  (u/printing
   (part-2 example-3)
   (part-2 input)))
