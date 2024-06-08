(ns day17
  (:require [clojure.string :as str]))

(def example 
  "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(def example-2
  "111111111111
999999999991
999999999991
999999999991
999999999991")

(def min-repeats 0)
(def max-repeats 4)

(defonce input (slurp "17.txt"))

(defn t-xs [x]
  (println (count x))
  (clojure.pprint/pprint x)
  (doall x))

(defn- char->long [c]
  (- (long c) 48))

(defn- parse [input]
  (mapv #(mapv char->long %) (str/split-lines input)))

;; directions
(defn- travel [[y x] dir]
  (case dir
    :r [y (inc x)]
    :d [(inc y) x]
    :l [y (dec x)]
    :u [(dec y) x]))

(def travel (memoize travel))

(def dirs [:r :d :l :u])

(defn- turns [dir]
  (if (nil? dir)
    [:r :d]
    (let [not-dir? (comp not #{dir})
          dir-loop (concat dirs dirs)
          dir-loop (if (= dir (first dir-loop))
                     (rest dir-loop)
                     dir-loop)]
      [(last (take-while not-dir? dir-loop))
       (second (drop-while not-dir? dir-loop))])))

(def turns (memoize turns))

(defn- next-dirs [dir repeat]
  (cond-> []
          (or (nil? dir) (>= repeat min-repeats))
          (into (turns dir))

          (and (some? dir) (< repeat max-repeats))
          (conj dir)))

(def next-dirs (memoize next-dirs))

(comment
  (next-dirs :u 2)
  (next-dirs :u 3)

  (next-dirs :u 2)
  (next-dirs :u 4)
  (next-dirs :u 9)
  (next-dirs :u 10))

(defn step-dir [grid {:keys [pos dir repeat loss]} new-dir]
  (let [h       (count grid)
        w       (count (first grid))
        pos'    (travel pos new-dir)
        cost    (get-in grid pos')
        [y x]   pos'
        repeat' (if (= dir new-dir) (inc repeat) 0)]
    (when (and (>= x 0) (< x w)
               (>= y 0) (< y h)
               (< repeat' max-repeats))
      {:pos    pos'
       :dir    new-dir
       :repeat repeat'
       :loss   (+ cost loss)})))

(defn- step
  "Calculate the next possible steps from the given state."
  [grid {:keys [dir repeat] :as state}]
  (remove nil?
          (for [dir (next-dirs dir repeat)]
            (step-dir grid state dir))))

(defn- redundant?
  "Have we seen this state, or  better one, before?"
  [best {:keys [pos dir repeat loss]}]
  (some
    #(when-let [prev-loss (get-in best [pos dir %])]
       (>= loss prev-loss))
    (range repeat (dec min-repeats) -1)))

(comment
  (redundant? {[0 0] {:u {1 29}}}
              {:pos    [0 0]
               :dir    :u
               :repeat 0
               :loss   20}))

(defn update-best [best {:keys [pos dir repeat loss] :as state}]
  (if (redundant? best state)
    best
    ;; remove certainly-inferior states
    (reduce
      (fn [acc r]
        (or (when-let [prev-loss (get-in best [pos dir r])]
              (when (>= prev-loss loss)
                (update-in acc [pos dir] dissoc r)))
            acc))
      (assoc-in best [pos dir repeat] loss)
      (range (max (inc min-repeats) (inc repeat)) max-repeats))))

(comment
  (update-best {[0 0] {:u {1 29}}}
               {:pos    [0 0]
                :dir    :u
                :repeat 2
                :loss   28}))

(def init
  {:pos    [0 0]
   ;; we don't want to limit how far we can go down or right
   :dir    nil
   :repeat 0
   :loss   0})


(defn best-loss [grid best]
  (let [h (count grid)
        w (count (first grid))]
    (->> (get best [(dec h) (dec w)])
         vals
         t-xs
         (mapcat #(keep (fn [[k v]]
                          (if (>= k min-repeats)
                            v))
                        %))
         t-xs
         (reduce min Long/MAX_VALUE))))

(defn solve-generic [input]
  (let [grid (parse input)]
    (loop [best        {}
           next-states [init]
           max-iters   400]
      (prn max-iters (count next-states))
      (if (zero? max-iters)
        (do (prn :too-many-steps)
            (clojure.pprint/pprint
              (clojure.walk/postwalk
                #(if (map? %)
                   (into (sorted-map) %)
                   %)
                best)))
        (if-let [next-states' (->> next-states
                                   (mapcat (partial step grid))
                                   (remove nil?)
                                   distinct
                                   (remove (partial redundant? best))
                                   doall
                                   seq)]
          (recur (reduce update-best best next-states)
                 next-states'
                 (dec max-iters))
          (best-loss grid best))))))

(defn part1 [input]
  (alter-var-root (resolve 'min-repeats) (constantly 0))
  (alter-var-root (resolve 'max-repeats) (constantly 3))
  (solve-generic input))

(defn part2 [input]
  (alter-var-root (resolve 'min-repeats) (constantly 3))
  (alter-var-root (resolve 'max-repeats) (constantly 10))
  (prn min-repeats max-repeats)
  (solve-generic input))

(comment
  (time (part1 example))
  (time (part1 in))

  (time (part2 example))
  (time (part2 example-2))
  (time (part2 input)))
