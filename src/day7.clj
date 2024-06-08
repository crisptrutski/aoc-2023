(ns day7
  (:require [utils :as u]))

(def example
  "32T3K 765
   T55J5 684
   KK677 28
   KTJJT 220
   QQQJA 483")

(defonce input (slurp "7.txt"))

(defn parse-line [s]
  (-> (u/split-ws s)
      (update 1 parse-long)))

(defn score-card [c]
  (u/index-of c "AKQJT987654321"))

(defn score-hand [fc sc]
  (u/match-idx
    (= 5 fc)
    (= 4 fc)
    (and (= 3 fc) (= 2 sc))
    (= 3 fc)
    (and (= 2 fc) (= 2 sc))
    (= 2 fc)))

(defn rank-hand [hand]
  (let [[fc sc] (reverse (sort (vals (frequencies hand))))]
    (score-hand fc sc)))

(defn rank-bid [[hand score]]
  [(rank-hand hand)
   (mapv score-card hand)
   score
   hand])

(defn part-1 [input]
  (->> (u/lines input)
       (map parse-line)
       (map rank-bid)
       (u/sort-desc)
       (map-indexed (fn [i [_ _ bid]] (* (inc i) bid)))
       (reduce +)))

;; ---

(defn score-card-2 [c]
  (u/index-of c "AKQT987654321J"))

(defn rank-hand-2 [hand]
  (let [hand-without-J (remove #{\J} hand)
        num-J (- 5 (count hand-without-J))
        [fc sc] (u/sort-desc (vals (frequencies hand-without-J)))
        fcj (+ num-J (or fc 0))]
    (score-hand fcj sc)))

(defn rank-bid-2 [[hand score]]
  [(rank-hand-2 hand)
   (mapv score-card-2 hand)
   score
   hand])

(defn part-2 [input]
  (->> (u/lines input)
       (map parse-line)
       (map rank-bid-2)
       (u/sort-desc)
       (map-indexed (fn [i [_ _ bid]] (* (inc i) bid)))
       (reduce +)))

(u/printing
  (part-1 example)
  (part-1 input))

(println '----)

(u/printing
  (part-2 example)
  (part-2 input))
