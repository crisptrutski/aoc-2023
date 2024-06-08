(ns day10
  (:require [clojure.string :as str]
            [utils :as u]))

(def example-1-clean
  ".....
   .S-7.
   .|.|.
   .L-J.
   .....")

(def example-2-clean
  "..F7-
   .FJ|7
   SJLL7
   |F--J
   LJ.LJ")

(def example-1
  "-L|F7
   7S-7|
   L|7||
   -L-J|
   L|-JF")

(def example-2
  "7-F7-
   .FJ|7
   SJLL7
   |F--J
   LJ.LJ")

(def every-dir
  #{[-1 0] [1 0] [0 -1] [0 1]})

(def pipe-dirs
  {\| #{[-1 0] [1 0]}
   \- #{[0 -1] [0 1]}
   \L #{[-1 0] [0 1]}
   \J #{[-1 0] [0 -1]}
   \7 #{[0 -1] [1 0]}
   \F #{[0 1] [1 0]}
   \. #{}
   \S every-dir})

;(doseq [[d adj] dirs]
;  (println d)
;  (let [adj? (set adj)]
;    (doseq [i [-1 0 1]]
;      (println (apply str
;                      (for [j [-1 0 1]]
;                        (if (adj? [i j]) "x" ".")))))))

(def input (slurp "10.txt"))


(def ^:dynamic *debug-grid* false)

(def *debug-grid (atom nil))

(defn update-grid! [next dist]
  (swap! *debug-grid
         (fn [grid]
           (reduce
             (fn [grid [y x]]
               (assoc-in grid [y x] dist))
             grid
             next))))

(defn print-grid [grid]
  (doseq [row grid]
    (println (apply str (map #(if (= \z %) \  %) row)))))

(defn print-grid! []
  (print-grid @*debug-grid))



(defn parse-grid [i]
  (mapv #(vec %) (u/lines i)))

(defn find-start [grid]
  (first
    (keep-indexed
      (fn [i row]
        (when-let [j (u/index-of \S row)]
          [i j]))
      grid)))

(defn neighbours [grid dirs [y x]]
  (for [[dy dx] (get dirs (u/get-2d grid y x))]
    [(+ y dy) (+ x dx) (- dy) (- dx)]))

(defn connected? [grid [uy ux udy udx]]
  (some (fn [[y d dy dx]]
          (and (= udy dy)
               (= udx dx)))
        (mapv (fn [[dy dx]]
                [(+ uy dy) (+ ux dx) dy dx])
              (get pipe-dirs (u/get-2d grid uy ux)))))

(defn strip-from [[y x _dy _dx]]
  [y x])

(defn part-1 [i]
  (let [grid (parse-grid i)
        start (find-start grid)]

    (reset! *debug-grid grid)

    (loop [visited #{}
           current (list start)
           len 0]

      (when *debug-grid*
        (update-grid! current len)
        (print-grid!)
        (println '-----))

      (let [visited (into visited current)
            neighbours (mapcat (partial neighbours grid pipe-dirs) current)
            connections (remove (comp visited strip-from) neighbours)
            next (filter (partial connected? grid) connections)]
        (if (empty? next)
          len
          (recur visited
                 (map strip-from next)
                 (inc len)))))))

(def example-3
  "...........
   .S-------7.
   .|F-----7|.
   .||.....||.
   .||.....||.
   .|L-7.F-J|.
   .|..|.|..|.
   .L--J.L--J.
   ...........")

(defn insert-gaps [grid]
  (let [width (count (first grid))
        new-w (+ width (dec width))]
    (vec (interpose
           (vec (repeat new-w \z))
           (for [row grid]
             (vec (interpose \z row)))))))

(defn connects-pipes-x? [grid y x]
  (let [l (u/get-2d grid y (dec x))
        r (u/get-2d grid y (inc x))]
    ;; BUG: only blocked by S if there's a connecting pipe adjacent
    (or (contains? #{\S \- \L \F} l)
        (contains? #{\S \- \J \7} r))))

(defn connects-pipes-y? [grid y x]
  (let [u (u/get-2d grid (dec y) x)
        d (u/get-2d grid (inc y) x)]
    ;; BUG: only blocked by S if there's a connecting pipe adjacent
    (or (contains? #{\S \| \7 \F} u)
        (contains? #{\S \| \L \J} d))))

(defn expand-grid [grid]
  (let [grid (insert-gaps grid)]
    (reduce
      (fn [grid [y x]]
        (let [c (u/get-2d grid y x)
              n (cond
                  (not= \z c) c
                  (connects-pipes-x? grid y x) \-
                  (connects-pipes-y? grid y x) \|
                  :else c)]
          (if (= c n)
            grid
            (assoc-in grid [y x] n))))
      grid
      (for [i (range (count grid))
            j (range (count (first grid)))]
        [i j]))))

(def spaces ".z")

(def space-dirs (zipmap spaces (repeat every-dir)))

(def space? (set spaces))

(defn find-spaces [grid]
  (reduce
    into
    #{}
    (keep-indexed
      (fn [i row]
        (keep-indexed
          (fn [j c]
            (when (= \. c)
              [i j]))
          row))
      grid)))

(defn connected-to-space? [grid [y x _ _]]
  (space? (u/get-2d grid y x)))

;; flood fill approach

;; enumerate all the dot positions
;; for-each, start building their groups by expanding
;; "expand" along pipes
;; => trick for pipes: insert "spaces" into grid.
;;    you can move along spaces, but they don't count towards size

;; can strip them out in final steps as they are counted

(defn border? [grid [y x]]
  (or (zero? y)
      (zero? x)
      (= (dec (count grid)) y)
      (= (dec (count (first grid))) x)))

(defn flood-fill [grid prev-visited start-pos]

  (reset! *debug-grid grid)

  (when-not (contains? prev-visited start-pos)
    (loop [visited (transient #{})
           current (list start-pos)]

      (when *debug-grid*
        (update-grid! current \*)
        (print-grid!)
        (println '-----))

      (let [visited (u/into! visited current)
            neighbours (mapcat (partial neighbours grid space-dirs) current)
            connections (remove (comp prev-visited strip-from) neighbours)
            connections (remove (comp visited strip-from) connections)
            next (filter (comp (partial connected-to-space? grid)) connections)]
        (if (empty? next)
          (persistent! visited)
          (recur visited
                 (map strip-from next)))))))

(defn group-spaces [grid spaces]
  (first
    (reduce
      (fn [[groups visited] pos]
        (let [new-group (flood-fill grid visited pos)]
          (if new-group
            [(conj groups new-group)
             (u/into! visited new-group)]
            [groups visited])))
      [[] (transient #{})]
      spaces)))


(defn print-grid-groups-char [grid groups char]
  (print-grid
    (first (reduce
             (fn [[grid i] group]
               (let [grid (reduce (fn [grid pos]
                                    (if (= \. (u/get-2d grid pos))
                                      (assoc-in grid pos char)
                                      grid))
                                  grid
                                  group)]
                 [grid (inc i)]))
             [grid 0]
             groups))))

(defn print-grid-groups [grid groups]
  (print-grid
    (first (reduce
             (fn [[grid i] group]
               (let [grid (reduce (fn [grid pos]
                                    (if (= \. (u/get-2d grid pos))
                                      (assoc-in grid pos (mod i 10))
                                      grid))
                                  grid
                                  group)]
                 [grid (inc i)]))
             [grid 0]
             groups))))

(defn count-spaces [grid group]
  (count (filter (comp #{\.})
                 (map (partial u/get-2d grid) group))))

(defn touches-border? [grid group]
  (some #(border? grid %) group))

(defn part-2 [i]
  (let [grid (parse-grid i)
        grid (expand-grid grid)
        spaces (find-spaces grid)]

    (let [groups (group-spaces grid spaces)]
      ;(println (remove (first groups) (second groups)))
      ;(println (remove (second groups) (first groups)))

      ;(clojure.pprint/pprint groups)

      ;(print-grid grid)
      ;(prn (count groups))
      (print-grid-groups grid groups)
      ;(print-grid-groups-char grid groups \ )

      (prn (map (juxt
                  (partial touches-border? grid)
                  (partial count-spaces grid))
                groups))

      (prn (into {}
                 (map (fn [[border? groups]]
                        {(not border?)
                         (u/sum (map #(count-spaces grid %) groups))}))
                 (group-by (partial touches-border? grid)
                           groups)))

      (reduce
        (fn [num group]
          (+ num (if (touches-border? grid group)
                   0
                   (count-spaces grid group))))
        0
        groups))))

(comment
  (defmacro debug [& body]
    `(binding [*debug-grid* true]
       ~@body))

  (debug (part-1 example-1))
  (debug (part-1 example-2))
  (part-1 input)

  (print-grid (expand-grid (u/lines example-1)))
  (print-grid (expand-grid (u/lines example-2)))
  (print-grid (expand-grid (u/lines example-3)))

  (part-2 example-1-clean)
  (part-2 example-2-clean)
  (part-2 example-3)
  ;(debug (part-2 example-3))

  (let [grid (parse-grid input)
        all-pos (for [i (range (count grid))
                      j (range (count (first grid)))]
                  [i j])]
    (count-spaces
      grid
      all-pos))

  ;; too low 426
  (part-2 input))

(defn shrink-grid [i]
  (->> (u/lines i)
       (u/every-odd)
       (map #(apply str (u/every-odd %)))
       vec))

(comment
  (spit
    "10-debug-shrink.txt"
    (str/join "\n" (shrink-grid (slurp "10-debug.txt")))))