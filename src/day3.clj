(ns day3
  (:require
   [clojure.string :as str]))

;; state

(def *ids (atom 0))

(defn get-id! [] (swap! *ids inc))

;;; data and fns

(def example
  "467..114..
   ...*......
   ..35..633.
   ......#...
   617*......
   .....+.58.
   ..592.....
   ......755.
   ...$.*....
   .664.598..")

(def input (slurp "3.txt"))

(defn digit? [c] (<= 48 (int c) 57))

(defn sym? [c] (not (or (= \. c) (digit? c))))

(defn id-pair [x y] [x y])

(defn parse-with-pos [keep-char? parse-str lines]
  (mapcat identity
          (map-indexed
            (fn [y line]
              (->> (str/trim line)
                   (map-indexed id-pair)
                   (partition-by (comp keep-char? last))
                   (keep
                     (fn [pairs]
                       (if-let [num (parse-str (apply str (map second pairs)))]
                         [num
                          (set (for [x (map first pairs)] [y x]))])))))
            lines)))

(defn positions [keep-char? lines]
  (->> lines
       (parse-with-pos
         #(if (keep-char? %) (get-id!))
         (comp keep-char? first))
       (map second)
       (mapcat identity)))

(defn adjacent [x y]
  (for [dx [-1 0 1]
        dy [-1 0 1]]
    [(+ dx x) (+ dy y)]))

(defn adjacent-positions [positions]
  (set (mapcat #(apply adjacent %) positions)))

(comment
  (positions sym? (str/split-lines example))
  (adjacent-positions (str/split-lines example))
  (parse-with-pos digit? parse-long (str/split-lines example)))

(defn part-1 [input]
  (let [lines (str/split-lines input)
        touched? (adjacent-positions (positions sym? lines))]
    (->> (parse-with-pos digit? parse-long lines)
         (keep (fn [[num positions]]
                 (if (some touched? positions)
                   num)))
         (reduce +))))

(def gear? #{\*})

(comment
  (positions gear? (str/split-lines example)))

(defn part-2 [input]
  (let [lines (str/split-lines input)
        nums-and-pos (parse-with-pos digit? parse-long lines)]
    (->> (positions gear? lines)
         (map (fn [[x y]]
                (let [adj? (set (adjacent x y))]
                  (keep
                    (fn [[part-num xys]]
                      (if (some adj? xys)
                        part-num))
                    nums-and-pos))))
         (filter (comp #{2} count))
         (map (partial apply *))
         (reduce +))))

(comment
  (part-1 example)
  (part-1 input)

  (part-2 example)
  (part-2 input))
