(ns day23
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def example
  "#.#####################
   #.......#########...###
   #######.#########.#.###
   ###.....#.>.>.###.#.###
   ###v#####.#v#.###.#.###
   ###.>...#.#.#.....#...#
   ###v###.#.#.#########.#
   ###...#.#.#.......#...#
   #####.#.#.#######.#.###
   #.....#.#.#.......#...#
   #.#####.#.#.#########v#
   #.#...#...#...###...>.#
   #.#.#v#######v###.###v#
   #...#.>.#...>.>.#.###.#
   #####v#.#.###v#.#.###.#
   #.....#...#...#.#.#...#
   #.#########.###.#.#.###
   #...###...#...#...#.###
   ###.###.#.###v#####v###
   #...#...#.#.>.>.#.>.###
   #.###.###.#.###.#.#v###
   #.....###...###...#...#
   #####################.#")

(def input (slurp "23.txt"))

(def start-pos [0 1])

(def ^:dynamic *end-pos*)

(defn- dirs* [c]
  (case c
    \. #{:l :r :u :d}
    \^ #{:u}
    \> #{:r}
    \v #{:d}
    \< #{:l}))

(def ^:dynamic *dirs* dirs*)

(defn- dir->delta [c]
  (case c
    :l [0 -1]
    :r [0 1]
    :u [-1 0]
    :d [1 0]))

(defn ->cell [cells pos]
  (when-let [c (cells pos)]
    [pos c]))

(defn move [pos dir]
  (mapv + pos (dir->delta dir)))

(defn moves [[pos c]]
  (for [d (*dirs* c)]
    (move pos d)))

(defn parse [s]
  (->> (str/split-lines s)
       (map str/trim)
       (map-indexed (fn [y l] (->> l (map-indexed (fn [x c]
                                                    [[y x] c])))))
       (reduce concat)
       (remove (comp #{\#} last))
       (into {})))

(defn- next-steps
  "Advance an explorer to all his potential next states."
  [{:keys [dist left cell]}]
  (for [pos (moves cell)
        :let [cell' (->cell left pos)]
        :when cell']
    {:dist (inc dist)
     :left (dissoc left pos)
     :cell cell'}))

#_(defn visualize-route [input-string left]
    (let [lines           (mapv (comp vec str/trim) (str/split-lines input-string))
          height          (count lines)
          width           (count (first lines))
          all-positions   (for [y (range height)
                                x (range width)]
                            [y x])
          route-positions (set (remove (set (map first left)) all-positions))
          char-at         (fn [[y x]]
                            (let [c (get-in lines [y x])]
                              (if (= c \#)
                                \#
                                (if (contains? route-positions [y x])
                                  \O
                                  c))))]
      (->> all-positions
           (group-by first)
           (sort-by first)
           (map (fn [[_ row-positions]]
                  (apply str (map char-at row-positions))))
           (str/join "\n")
           println)))

;;;  Naive algorithm
(defn explore
  "Recursively explore each route as far as possible, aggregating and then returning the furthest distance"
  [{:keys [best routes left]}]
  (if-let [r (peek routes)]
    (if-let [nxt (seq (next-steps r))]
      (recur
       {:best   best
        :left   left
        :routes (into (pop routes) nxt)})
      (if (= *end-pos* (first (:cell r)))
        (recur
         {:best   (max best (:dist r))
          :left   (if (>= (:dist r) best) (:left r) left)
          :routes (pop routes)})
        (recur
         {:best   best
          :left   left
          :routes (pop routes)})))
    best))

;;;  Optimized

(defn find-junctions [cells]
  (let [directions #{:l :r :u :d}]
    (->> cells
         (filter (fn [[pos _]]
                   (or (= pos start-pos)
                       (= pos *end-pos*)
                       (> (count (filter #(cells (move pos %)) directions)) 2))))
         (map first)
         set)))

(defn explore-path
  "Find the distances to each reachable junction when starting off from a given direction from a given point."
  [cells start dir junctions]
  (loop [cell     (->cell cells (move start dir))
         distance 1
         visited  #{(->cell cells start)}]
    (if-let [jx (junctions (first cell))]
      {:junction jx, :distance distance}
      (let [next-cells (when cell (keep (partial ->cell cells) (moves cell)))
            next-moves (set/difference (set next-cells) visited)]
        (when (= 1 (count next-moves))
          (recur (first next-moves)
                 (inc distance)
                 (conj visited cell)))))))

(defn build-junction-graph [cells]
  (let [junctions             (find-junctions cells)
        explore-from-junction (fn [junction]
                                (->> (*dirs* (cells junction))
                                     (keep #(explore-path cells junction % junctions))
                                     (map (juxt :junction :distance))
                                     (into {})))]
    (into {} (map (juxt identity explore-from-junction) junctions))))

(defn find-longest-path [junction-graph]
  (letfn [(dfs [current-pos visited distance]
            (if (= current-pos *end-pos*)
              distance
              (->> (junction-graph current-pos)
                   (remove #(contains? visited (key %)))
                   (map (fn [[next-pos dist]]
                          (dfs next-pos
                               (conj visited next-pos)
                               (+ distance dist))))
                   (apply max -1))))]
    (dfs start-pos #{start-pos} 0)))

(defn part-1 [s]
  (let [cells (parse s)]
    (binding [*end-pos* [(dec (count (str/split-lines s))) (dec (dec (count (first (str/split-lines s)))))]]
      (find-longest-path (build-junction-graph cells))
      #_(explore {:best   0
                  :routes (list {:dist 0
                                 :left cells
                                 :cell (->cell cells start-pos)})}))))

(defn part-2 [s]
  ;; dry the slopes
  (binding [*dirs* (constantly (dirs* \.))]
    (part-1 s)))

(comment
  (parse example)

  (part-1 example)
  (part-1 input)

  (part-2 example)
  (part-2 input))
