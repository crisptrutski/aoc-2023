(ns utils
  (:require [clojure.string :as str]))

(defn tr [x] (clojure.pprint/pprint x) x)

(defn index-of [x xs]
  (first (keep-indexed (fn [i y] (if (= x y) i)) xs)))

(defn split-ws [s]
  (str/split (str/trim s) #"\s+"))

(defn lines [s]
  (map str/trim (str/split-lines s)))

(defn sum [xs]
  (reduce + 0 xs))

(defn sort-desc [xs]
  ;; todo - rather use custom comparator for speed
  (reverse (sort xs)))

(defn capture
  ([re] (partial capture re))
  ([re s] (second (re-find re s))))

(defmacro match-idx [& preds]
  (let [indexed (map-indexed (fn [i p] [p i]) preds)]
    `(cond ~@(concat (apply concat indexed) [:else (count preds)]))))
