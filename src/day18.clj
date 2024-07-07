(ns day18
  (:require
   [clojure.string :as str]
   [utils :as u]))

(def example
  "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

(def input "" #_(slurp "18.txt"))

(defn parse-line [line]
  (let [[_ dir len col] (re-matches #"(\w) (\d*) \(#([0-9a-f]*)\)" line)]
    [(keyword dir)
     (parse-long len)
     (Long/parseLong col 16)]))

(defn parse [text]
  (->> text str/split-lines (mapv parse-line)))

(defn- move
  ([pos dir]
   (case dir
     :U (update pos 0 dec)
     :R (update pos 1 inc)
     :D (update pos 0 inc)
     :L (update pos 1 dec)))
  ([pos dir n]
   (case dir
     :U (update pos 0 (fnil - 0 n) nil)
     :R (update pos 1 (fnil + 0 n) nil)
     :D (update pos 0 (fnil + 0 n) nil)
     :L (update pos 1 (fnil - 0 n) nil))))

(defn- dig-trenches* [dig instructions]
  (first (reduce
          (fn [state instruction]
            (reduce
             (fn [[holes _] pos]
               [(conj holes pos) pos])
             state
             (dig (second state) instruction)))
          #_[#{[0 0]} [0 0]]
          [[[0 0]] [0 0]]
          instructions)))

(defn- dig-line [pos [dir len _]]
  (first
   (reduce
    (fn [[line pos] _]
      (let [pos' (move pos dir)]
        [(conj line pos') pos']))
    [[] pos]
    (range len))))

(defn- dig-trenches [instructions]
  (dig-trenches* dig-line instructions))

(defn- area [grid]
  (reduce + (mapcat #(map (fn [c] (if c 1 0)) %) grid)))

(defn- neighbours [[i j]]
  [[(inc i) j]
   [(dec i) j]
   [i (inc j)]
   [i (dec j)]])

(defn- fill-interior [grid]
  ;; guessing how flood fill works
  (let [visited? (volatile! #{})
        mapping  (volatile! {})
        next-id  (volatile! 0)
        h        (count grid)
        w        (count (first grid))
        range?   (fn [[i j]]
                   (and (>= i 0) (< i h)
                        (>= j 0) (< j w)))
        edge?    (fn [[i j]]
                   (or (= i 0) (= i (dec h))
                       (= j 0) (= j (dec w))))
        dug?     #(get-in grid %)
        ground?  (comp not dug?)
        result   (volatile! grid)]
    (doseq [i (range h)
            j (range w)
            :let [pos [i j]]
            :when (and (ground? pos)
                       (not (@visited? pos)))
            :let [id (vswap! next-id inc)]]
      (loop [exterior? false
             next      [pos]]
        (when (seq next)
          (vswap! visited? into next)
          (let [just-opened? (when-not exterior? (some edge? next))
                exterior?    (or exterior? just-opened?)]
            (when just-opened?
              (vswap! mapping dissoc id))
            (when-not exterior?
              (vswap! mapping update id into next))
            (let [new? (comp not @visited?)]
              (recur exterior?
                     (filter (every-pred range? new? ground?)
                             (distinct (mapcat neighbours next)))))))))
    (reduce
     (fn [grid [id positions]]
       (reduce #(assoc-in %1 %2 id) grid positions))
     @result
     @mapping)))

(defn holes->grid [holes]
  (let [xs    (map second holes)
        ys    (map first holes)
        min-x (reduce min xs)
        max-x (reduce max xs)
        min-y (reduce min ys)
        max-y (reduce max ys)
        w     (inc (- max-x min-x))
        h     (inc (- max-y min-y))
        grid  (vec (repeat h (vec (repeat w false))))]
    (reduce
     (fn [holes [y x]]
       (let [i (- y min-y)
             j (- x min-x)]
         (assoc-in holes [i j] true)))
     grid
     holes)))

(defn- print-grid [grid]
  (dorun (mapv (fn [row]
                 (->> row
                      (map #(cond (int? %) %
                                  % \#
                                  :else \.))
                      (apply str)
                      (println)))
               grid)))

(defn text->grid [text]
  (->> (u/lines text)
       (mapv #(mapv #{\#} %))))

(defn part1 [text]
  (-> text
      parse
      dig-trenches
      holes->grid
      fill-interior
      area))

(defn- dig-intersection [pos [dir len _col]]
  [(move pos dir len)])

(defn- dig-intersections [instructions]
  (dig-trenches* dig-intersection instructions))

(defn- dig-hex-intersection [pos [_ _ col]]
  (let [len (quot col 16)
        dir (case (mod col 16)
              0 :R
              1 :D
              2 :L
              3 :U)]
    [(move pos dir len)]))

(defn- dig-hex-intersections [instructions]
  (dig-trenches* dig-hex-intersection instructions))

(defn shoelace-area [positions]
  (let [xs        (map second positions)
        ys        (map first positions)
        laced     (abs (- (reduce + (map * xs (rest (cycle ys))))
                          (reduce + (map * (rest (cycle xs)) ys))))
        perimeter (->> positions
                       (cons (last positions))
                       (partition 2 1)
                       (map #(apply utils/manhattan %))
                       (reduce +))]
    (inc (quot (+ laced perimeter) 2))))

(defn- fast-area [text intersections]
  (-> text parse intersections shoelace-area))

(defn part2 [text]
  (fast-area text dig-hex-intersections))

(comment
  (part1 example)
  (part1 input)

  ;; faster version of part1
  (fast-area example dig-intersections)
  (fast-area input dig-intersections)

  (part2 example)
  (part2 input))
