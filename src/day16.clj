(ns day16
  (:require [clojure.string :as str]))

(def example 
  ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....
")

(defonce input (slurp "16.txt"))

(defn- parse [input]
  (mapv vec (str/split-lines input)))

(defn- print-activations [activations]
  (doseq [row activations]
    (apply println (for [a row]
                     (if (pos? a) \# \.)))))

(defn- count-activations
  "How many squares are being illuminated?"
  [activations]
  (transduce (comp (mapcat identity)
                   (filter pos?)
                   (map (constantly 1)))
             +
             activations))

(defn in-bounds?
  "Is this beam within the bounds of the grid?"
  [grid [[y x] _]]
  (and (>= y 0)
       (>= x 0)
       (< y (count grid))
       (< x (count (first grid)))))

(defn- repeat?
  "Has a beam travelled through in this direction before?"
  [activations [[y x] dir :as _beam]]
  (pos? (bit-and dir (get-in activations [y x]))))

;; directions
(def r 1)
(def d 2)
(def l 4)
(def u 8)

(defn- traveling? [dirs dir]
  (pos? (bit-and dirs dir)))

(defn- travel [[y x] dir]
  (case dir
    1 [[y (inc x)] r]
    2 [[(inc y) x] d]
    4 [[y (dec x)] l]
    8 [[(dec y) x] u]))

(defn- add-beam [beams pos dir]
  (conj beams (travel pos dir)))

(defn- propagate [pos dirs mapping]
  (reduce
    (fn [beams [in out]]
      (if (traveling? dirs in)
        (add-beam beams pos out)
        beams))
    {}
    mapping))

(defn- next-beams [grid activations [[y x :as pos] dirs]]
  (->> (case (get-in grid pos)
         \. (propagate pos dirs {r r, d d, l l, u u})
         \- {[y (dec x)] l
             [y (inc x)] r}
         \| {[(inc y) x] d
             [(dec y) x] u}
         \/ (propagate pos dirs {r u, d l, l d, u r})
         \\ (propagate pos dirs {r d, d r, l u, u l}))
       (filter (partial in-bounds? grid))
       (remove (partial repeat? activations))
       (reduce (fn [acc [[y x] dir]]
                 (update acc [y x] (fnil (partial bit-or dir) 0)))
               {})))

(defn- track-beam
  "Update the activations to track a beam going through this position in this direction."
  [activations [[y x] dirs]]
  (update-in activations [y x] bit-or dirs))

(defn- track-beams [activations beams]
  (reduce track-beam activations beams))

(defn- step
  "Calculate any novel positions the beams travel next, and update the activations"
  [grid [activations beams]]
  (reduce
    (fn [[acc-tivations acc-beams] beam]
      (let [triggered-beams (next-beams grid acc-tivations beam)]
        [(track-beams acc-tivations triggered-beams)
         (merge-with bit-or acc-beams triggered-beams)]))
    [activations {}]
    beams))

(defn- activations-fixed-point [step-once state]
  (loop [state state]
    (let [next-state (step-once state)
          old-activations (first state)
          new-activations (first next-state)]
      (if (= old-activations new-activations)
        old-activations
        (recur next-state)))))

(comment
  (let [grid [[\. \.]
              [\. \.]]]
    (->> [[[1 0 0]
           [0 0 0]]
          {[0 0] 1}]
         (activations-fixed-point (partial step grid)))))

(defn init
  ([grid]
   (init grid {[0 0] 1}))
  ([grid initial-beams]
   (let [no-activations (mapv (partial mapv (constantly 0)) grid)]
     [(track-beams no-activations initial-beams)
      initial-beams])))

(defn part1 [input]
  (let [grid (parse input)]
    (->> (init grid)
         (activations-fixed-point (partial step grid))
         count-activations)))

(defn- possible-init-beams [grid]
  (let [h (count grid)
        w (count (first grid))]
    (flatten (concat (for [x (range w)]
                       [{[0 x] d}
                        {[(dec h) x] u}])
                     (for [y (range h)]
                       [{[y 0] r}
                        {[y (dec w)] l}])))))

(defn part2 [input]
  (let [grid (parse input)]
    (reduce max (for [init-beams (possible-init-beams grid)]
                  (->> (init grid init-beams)
                       (activations-fixed-point (partial step grid))
                       count-activations)))))

(comment
  (part1 example)
  (part1 input)

  (part2 example)
  (part2 input)

  (print-activations (first (init (parse example) {[0 0] 1}))))