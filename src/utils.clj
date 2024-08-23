(ns utils
  (:require
   [clojure.string :as str]))

(defn pp [x] #p x)

(defn into! [xs ys]
  (reduce conj! xs ys))

(defn index-of [x xs]
  (first (keep-indexed (fn [i y] (when (= x y) i)) xs)))

(defn every-odd [xs]
  (map first (partition-all 2 xs)))

(defn split-ws [s]
  (str/split (str/trim s) #"\s+"))

(defn lines [s]
  (map str/trim (str/split-lines s)))

(defn get-2d
  ([grid [y x]]
   (get-2d grid y x))
  ([grid y x]
   (-> grid (nth y []) (nth x nil))))

(defn sum [xs]
  (reduce + xs))

(defn gcd [n d]
  (if (> d n)
    (recur d n)
    (if (zero? d)
      (abs n)
      (recur d (mod n d)))))

(defn lcm [n d]
  (quot (abs (* n d)) (gcd n d)))

(defn flip [f]
  #(f %2 %1))

(defn sort-desc [xs]
  (sort (flip compare) xs))

(defn capture
  ([re] (partial capture re))
  ([re s] (second (re-find re s))))

(defn ending-with [e]
  (fn [s]
    (str/ends-with? s e)))

(defn manhattan
  "The manhattan distance between two points"
  [[y1 x1] [y2 x2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defmacro match-idx
  "Given a list of predicates, turn the index of the first predicate that passes."
  [& preds]
  (let [indexed (map-indexed (fn [i p] [p i]) preds)]
    `(cond ~@(concat (apply concat indexed) [:else (count preds)]))))

(defmacro printing [& body]
  (cons 'do (for [b body] `(println ~b))))

(defn fixed-point [f s]
  (reduce (fn [lst nxt]
            (if (= lst nxt)
              (reduced nxt)
              nxt))
          ::none
          (iterate f s)))
