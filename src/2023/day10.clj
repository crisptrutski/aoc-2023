(ns day10
  (:require
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

(def input (slurp "10.txt"))

(def ^:dynamic *debug-grid* false)

(defmacro debug [& body]
  `(binding [*debug-grid* true]
     ~@body))

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
    [(+ y dy)
     (+ x dx)
     ;; can't recall why this is inverted
     (- dy)
     (- dx)]))

(defn connected? [grid [uy ux udy udx]]
  (some (fn [[_y _x dy dx]]
          (and (= udy dy)
               (= udx dx)))
        (mapv (fn [[dy dx]]
                [(+ uy dy) (+ ux dx) dy dx])
              (get pipe-dirs (u/get-2d grid uy ux)))))

(defn strip-from [[y x _dy _dx]]
  [y x])

(defn part-1 [i]
  (let [grid  (parse-grid i)
        start (find-start grid)]

    (reset! *debug-grid grid)

    (loop [visited #{}
           current (list start)
           len     0]

      (when *debug-grid*
        (update-grid! current len)
        (print-grid!)
        (println '-----))

      (let [visited     (into visited current)
            neighbours  (mapcat (partial neighbours grid pipe-dirs) current)
            connections (remove (comp visited strip-from) neighbours)
            next        (filter (partial connected? grid) connections)]
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

(def example-3b
  "..........
   .S------7.
   .|F----7|.
   .||....||.
   .||....||.
   .|L-7F-J|.
   .|..||..|.
   .L--JL--J.
   ..........")

(defn infer-start [grid [y x]]
  ;; assume point is not on the edge of the grid
  (let [u (get-in grid [(dec y) x])
        d (get-in grid [(inc y) x])
        l (get-in grid [y (dec x)])
        r (get-in grid [y (inc x)])]
    (cond
      (and ((set "7J-") r)
           ((set "LJ|") d)) \F
      (and ((set "7J-") r)
           ((set "F7|") u)) \L
      (and ((set "LF-") l)
           ((set "LJ|") d)) \7
      (and ((set "LF-") l)
           ((set "F7|") u)) \J
      (and ((set "F7|") u)
           ((set "LJ|") d)) \|
      (and ((set "LF-") l)
           ((set "7J-") r)) \-)))

(defn- clear-disconnected [grid start]
  (loop [visited #{}
         current (list start)]
    (let [visited     (into visited current)
          neighbours  (mapcat (partial neighbours grid pipe-dirs) current)
          connections (remove (comp visited strip-from) neighbours)
          next        (filter (partial connected? grid) connections)]
      (if (empty? next)
        (->> grid
             (map-indexed
              (fn [y row]
                (->> row
                     (map-indexed
                      (fn [x c]
                        (cond
                          (= \S c) (infer-start grid start)
                          (visited [y x]) c
                          :else \.)))
                     vec)))
             vec)
        (recur visited (map strip-from next))))))

(def tweak
  {\F 1
   \L 2
   \7 1
   \J 2
   \| 3})

(defn- mark-parity [grid]
  (mapv (fn [row]
          (second
           (reduce
            (fn [[x row-acc state] c]
              [(inc x)
               (conj row-acc (cond
                               (not= \. c) c
                               (zero? state) \.
                               :else \I))
               (bit-xor state (tweak c 0))])
            [0 [] 0]
            row)))
        grid))

(defn- count-internal [marked-grid]
  (transduce
   (map #(count (filter (set "I") %)))
   +
   0
   marked-grid))

(defn part-2 [s]
  (let [grid  (parse-grid s)
        start (find-start grid)]
    (-> (clear-disconnected grid start) mark-parity count-internal)))

(comment
  (defmacro debug [& body]
    `(binding [*debug-grid* true]
       ~@body))

  (debug (part-1 example-1))
  (debug (part-1 example-2))
  (part-1 input)

  (part-2 example-1-clean)
  (part-2 example-2)
  (part-2 example-2-clean)
  (part-2 example-3)
  (part-2 example-3b)

  (part-2 input))
