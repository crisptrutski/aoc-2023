(ns day21
  (:require
   [utils :as u]))

(def example
  "...........
   .....###.#.
   .###.##..#.
   ..#.#...#..
   ....#.#....
   .##..S####.
   .##..#...#.
   .......##..
   .##.#.####.
   .##..##.##.
   ...........")

(def input (slurp "21.txt"))

(defn- grid->cells [grid]
  (->> grid
       (map-indexed
        (fn [i row]
          (map-indexed (fn [j v] [i j v]) row)))
       (reduce concat [])
       vec))

(defn- positions [needle cells]
  (for [[i j v] cells
        :when   (= v needle)]
    [i j]))

(defn parse [i]
  (let [grid     (utils/split-ws i)
        cells    (grid->cells grid)
        starting (positions \S cells)]
    {:positions starting
     :options   (into (set (positions \. cells)) starting)}))

(defn move [[i j]]
  [[(inc i) j]
   [(dec i) j]
   [i (inc j)]
   [i (dec j)]])

(defn step-state [{:keys [positions options] :as state}]
  (assoc state :positions
         (into #{}
               (comp (mapcat move)
                     (filter options))
               positions)))

(defn- count-positions [state]
  (count (:positions state)))

(defn- n-steps [n state]
  (nth (iterate step-state state) n))

(defn part-1 [n i]
  (count-positions (n-steps n (parse i))))

(defn part-2 [n i]
  ;; tile the infinite plan with translational symmetry
  ;;
  ;; idea 1: treat translationally symmetric points as counts
  ;;  but we need to be careful that we don't double count the same points
  ;; idea 2: at certain point there are cycle-like things -> the grid beccomes a multiple of an earlier state?
  ;;   but that is really expensive to check
  ;; idea 3: if we can decompose a state into sum of previous states,
  ;;   then its evolution is the sum of their evolution
  ;; idea 4: the states form a sort of checkboard - once visited, each cell is active again 2 steps later
  )

(comment
  ;; 6 steps => 16 positions
  (part-1 6 example)
  ;; 64 steps => 3820 positions
  (part-1 64 input)

  ;; In exactly 5000 steps, he can reach 16733044 garden plots.
  (part-2 5000 example)

  (part-2 26501365 input))
