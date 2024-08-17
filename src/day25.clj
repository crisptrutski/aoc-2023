(ns day25
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

(def example
  "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr")

(def input (slurp "25.txt"))

(defn parse-graph [input]
  (reduce
   (fn [graph line]
     (let [[node & connections] (str/split (str/trim line) #":\s*|,\s*|\s+")]
       (reduce
        (fn [g conn]
          (-> g
              (update node (fnil conj #{}) conn)
              (update conn (fnil conj #{}) node)))
        graph
        connections)))
   {}
   (str/split-lines input)))

;;;  naive

(defn remove-edges [graph edges]
  (reduce (fn [g [u v]]
            (-> g
                (update u disj v)
                (update v disj u)))
          graph
          edges))

(defn dfs [graph start]
  (loop [stack   (list start)
         visited #{}]
    (if (empty? stack)
      visited
      (let [v     (first stack)
            stack (rest stack)]
        (if (visited v)
          (recur stack visited)
          (recur (into stack (graph v)) (conj visited v)))))))

(defn connected? [graph]
  (let [start (first (keys graph))]
    (= (set (keys graph)) (dfs graph start))))

(defn combinations [items n]
  (cond
    (= n 0)        '(())
    (empty? items) '()
    :else          (concat
                    (map #(cons (first items) %) (combinations (rest items) (dec n)))
                    (combinations (rest items) n))))

(defn find-three-edge-cut [graph]
  (let [edges (vec (for [[u conns] graph
                         v         conns
                         :when     (< (compare u v) 0)]
                     [u v]))]
    (first
     (filter
      (fn [three-edges]
        (not (connected? (remove-edges graph three-edges))))
      (combinations edges 3)))))

;;

(defn contract-edge [graph u v]
  (let [merged-node (str u "-" v)
        neighbors (-> (set/union (get graph u) (get graph v))
                      (disj u v))]
    (as-> graph g
      (dissoc g u v)
      (assoc g merged-node neighbors)
      (reduce (fn [g neighbor]
                (update g neighbor #(-> % (disj u v) (conj merged-node))))
              g
              neighbors))))

(defn random-edge [graph]
  (let [u (rand-nth (vec (keys graph)))
        v (rand-nth (vec (graph u)))]
    [u v]))

(defn karger-min-cut [graph]
  (loop [g graph]
    (if (= (count g) 2)
      [(first g) (second g)]
      (let [[u v] (random-edge g)]
        (recur (contract-edge g u v))))))

(defn find-cut-edges [original-graph [part1 part2]]
  (for [u (str/split (key part1) #"-")
        v (str/split (key part2) #"-")
        :when (contains? (original-graph u) v)]
    [u v]))

(defn find-three-edge-cut [graph]
  (loop [attempts 0
         max-attempts (* (count graph) (count graph) (Math/log (count graph)))]
    (if (>= attempts max-attempts)
      nil  ; No three-edge cut found after many attempts
      (let [[part1 part2] (karger-min-cut graph)
            cut-edges (find-cut-edges graph [part1 part2])]
        (if (= (count cut-edges) 3)
          (do
            (prn (* (count (str/split (key part1) #"-"))
                    (count (str/split (key part2) #"-"))))
            cut-edges)
          (recur (inc attempts) max-attempts))))))

;;

(def graph-1 (parse-graph example))

(def graph-2 (parse-graph input))

(comment
  (println "Three edges to remove:" (find-three-edge-cut graph-1))
  (println "Three edges to remove:" (find-three-edge-cut graph-2)))
