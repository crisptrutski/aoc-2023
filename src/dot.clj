(ns dot
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [day20])
  (:import
   [clojure.lang PersistentQueue]
   (java.awt Desktop)
   (java.io File)))

(defn graph-to-dot [graph-map]
  (str "digraph G {\n"
       (str/join "\n"
                 (for [[node neighbors] graph-map
                       neighbor         neighbors]
                   (format "  \"%s\" -> \"%s\";" node neighbor)))
       "\n}"))

(defn save-dot-file [dot-content filename]
  (spit filename dot-content))

(defn render-graph [graph-map dot-filename png-filename]
  (let [dot-content (graph-to-dot graph-map)]
    (save-dot-file dot-content dot-filename)
    (sh "dot" "-Tpng" dot-filename "-o" png-filename)))

(defn open-image [file-path]
  (let [desktop (Desktop/getDesktop)
        file    (File. file-path)]
    (.open desktop file)))

(def name->type
  (-> day20/input (#'day20/parse) :nodes
      (update-vals :type)))

(def day20-graph
  (-> day20/input
      (#'day20/parse)
      :nodes
      (update-vals (comp vec :destinations))
      (->> (walk/postwalk
            (fn [x]
              (if-let [t (name->type x)]
                (str x " " t)
                x))))))

(defn reverse-graph [graph]
  (reduce-kv
   (fn [acc node neighbors]
     (reduce
      (fn [m neighbor]
        (update m neighbor (fnil conj #{}) node))
      acc
      neighbors))
   {}
   graph))

(defn reachable-nodes [graph start]
  (loop [visited  #{}
         to-visit (conj PersistentQueue/EMPTY start)]
    (if (empty? to-visit)
      visited
      (let [current      (peek to-visit)
            neighbors    (get graph current [])
            new-to-visit (into (pop to-visit) (remove visited neighbors))]
        (recur (conj visited current) new-to-visit)))))

(defn filter-graph [graph nodes]
  (into {} (filter (fn [[k _]] (nodes k)) graph)))

(defn reverse-and-filter-graph [original-graph start-node]
  (let [reversed-graph (reverse-graph original-graph)
        reachable      (reachable-nodes reversed-graph start-node)]
    (filter-graph reversed-graph reachable)))

(def day20-rx (reverse-and-filter-graph day20-graph "rx"))

(render-graph day20-graph "day20.dot" "day20.png")
(open-image "day20.png")

(render-graph day20-rx "day20rx.dot" "day20rx.png")
(open-image "day20rx.png")
