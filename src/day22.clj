(ns day22
  (:require
   [clojure.string :as str]
   [utils :as u]
   [clojure.set :as set]))

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
  (assert (> 2 (+ (if (= x1 x2) 0 1)
                  (if (= y1 y2) 0 1)
                  (if (= z2 z2) 0 1)))
          "no diagonals")

  (into (sorted-set)
        (for [x (range-incl x1 x2)
              y (range-incl y1 y2)
              z (range-incl z1 z2)]
          [x y z])))

(defn- parse-brick [l]
  (->> (str/split l #"~")
       (mapv (comp #(map parse-long %)
                   #(str/split % #",")
                   str/trim))
       fill-bricks))

(defn parse [s]
  (mapv parse-brick (str/split-lines s)))

(defn highest-point
  "Find the highest point in the surface at given x and y coordinates."
  [surface x y]
  (get surface [x y] {:id nil :z 0}))

(defn update-surface
  "Update the surface with new brick positions."
  [surface id z' bricks]
  (reduce (fn [m [x y _]]
            (assoc m [x y] {:id id :z z'}))
          surface
          bricks))

(defn step
  "Takes the current state (surface and supporting information) and a brick,
   and returns the updated state after placing the brick, as well as a
   mapping from each brick to the bricks (if any) that are holding it up."
  [{:keys [surface _supporting] :as state} {:keys [id bricks]}]
  (let [[[_ _ z1] [_ _ z2]] [(first bricks) (last bricks)]
        {support-z :z support-ids :ids}
        (reduce
         (fn [{:keys [z ids] :as acc} [x y _]]
           (let [{surface-id :id surface-z :z} (highest-point surface x y)]
             (cond
               (> surface-z z) {:z surface-z :ids #{surface-id}}
               (= surface-z z) (update acc :ids conj surface-id)
               :else           acc)))
         {:z 0 :ids #{}}
         bricks)
        z' (+ support-z 1 (Math/abs (- z2 z1)))]
    (-> state
        (update :surface update-surface id z' bricks)
        (assoc-in [:supporting id] support-ids)))) (defn highest-point [surface x y]
                                                     (get surface [x y] {:brick nil :z 0}))

(defn supporting-nothing [supporting]
  (->> (keys supporting)
       (remove (into #{}
                     (comp (filter #(= 1 (count %)))
                           (map first))
                     (vals supporting)))))

(defn step
  [{:keys [surface supporting]}
   {:keys [id bricks]}]
  (let [support (reduce
                 (fn [{:keys [z ids] :as acc} [x y _]]
                   (let [{surface-id :id surface-z :z} (highest-point surface x y)]
                     (cond
                       (> surface-z z) {:z surface-z :ids [surface-id]}
                       (= surface-z z) {:z surface-z :ids (conj ids surface-id)}
                       :else           acc)))
                 {:z 0 :ids []}
                 bricks)
        z' (+ (:z support)
              1
              (Math/abs (- (last (last bricks))
                           (last (first bricks)))))
        surface' (reduce (fn [m [x y _]]
                           (assoc m [x y] {:id id :z z'}))
                         surface
                         bricks)]
    {:surface surface'
     :supporting (assoc supporting id (set (:ids support)))}))

(defn- support-map [s]
  (->> (parse s)
       (sort-by #(reduce min (map last %)))
       (map #(hash-map :id %1 :bricks %2) (map #(char (+ % (int \a))) (range)))
       (reduce step nil)
       :supporting))

(defn part-1 [s]
  (count (supporting-nothing (support-map s))))

(defn- cascade-size [supporting starting]
  (loop [max-depth 20000
         disintegrated #{starting}
         remaining (remove (comp disintegrated first) supporting)]
    (if (zero? max-depth)
      -1
      (if-let [non-supporting (not-empty (into #{}
                                               (comp
                                                (filter (fn [[_ supported-by]]
                                                          (every? disintegrated supported-by)))
                                                (map first))
                                               remaining))]
        (recur (dec max-depth)
               (set/union disintegrated non-supporting)
               (remove (comp non-supporting first) remaining))
        (dec (count disintegrated))))))

(defn sum-cascades [supporting]
  (reduce + (map
             #(cascade-size supporting %)
             (remove (set (supporting-nothing supporting))
                     (map first supporting)))))

(defn part-2 [s]
  (sum-cascades (support-map s)))

(comment
  (part-1 example)
  ;; 1359 -> too high
  (part-1 input)

  (part-2 example)
  ;; 1250 - too low
  (part-2 input))
