(ns day15
  (:require
   [clojure.string :as str]))

(def example "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defonce input (slurp "15.txt"))

(defn hsh [s]
  (loop [acc 0
         [n & ext] s]
    (if-not n
      acc
      (recur (rem (* 17 (+ acc (long n))) 256)
             ext))))

(defn part1 [input]
  (->> (str/split-lines input)
       (mapcat #(->> (str/split % #",")
                     (map hsh)))
       (reduce +)))

(defn replace-lens [lenses label focal-length]
  (reduce
   (fn [acc lens]
     (conj acc
           (if (= label (first lens))
             [label focal-length]
             lens)))
   []
   lenses))

(defn sum-box [[box-num lenses]]
  (* (inc box-num)
     (->> lenses
          (map (fn [i [_ fl]]
                 (* (inc i) fl))
               (range))
          (reduce +))))

(defn find-idx [lenses label]
  (loop [i 0
         [l & enses] lenses]
    (when l
      (if (= (first l) label)
        i
        (recur (inc i) enses)))))

(defn t [x] (clojure.pprint/pprint x) x)

(defn part2 [input]
  (->> (str/split-lines input)
       (mapcat #(str/split % #","))
       (reduce (fn [boxes s]
                 (let [[label focal-length] (str/split s #"\-|=")
                       focal-length (when focal-length
                                      (parse-long focal-length))
                       box (hsh label)]
                   (update boxes box
                           (fn [lenses]
                             (if (some #{\=} s)
                               (let [idx (find-idx lenses label)]
                                 (if idx
                                   (assoc lenses idx [label focal-length])
                                   (conj (vec lenses) [label focal-length])))
                               (into [] (remove (comp #{label} first)) lenses))))))
               {})
       (map sum-box)
       (reduce +)))

(comment
  (part1 example)
  (part1 input)

  (part2 example)
  (part2 input))
