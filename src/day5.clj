(ns day5
  (:require
   [clojure.string :as str]))

(def example
  "seeds: 79 14 55 13

   seed-to-soil map:
   50 98 2
   52 50 48

   soil-to-fertilizer map:
   0 15 37
   37 52 2
   39 0 15

   fertilizer-to-water map:
   49 53 8
   0 11 42
   42 0 7
   57 7 4

   water-to-light map:
   88 18 7
   18 25 70

   light-to-temperature map:
   45 77 23
   81 45 19
   68 64 13

   temperature-to-humidity map:
   0 69 1
   1 0 69

   humidity-to-location map:
   60 56 37
   56 93 4")

(def input (slurp "5.txt"))

(defn parse-label [s]
  (drop 1 (re-find #"(\w+)-to-(\w+)" s)))

(defn parse-seeds [s]
  (remove nil? (map parse-long (str/split s #"\s+"))))

(defn expand-range [dest-start source-start len]
  (map vector (range source-start (+ source-start len))
       (range dest-start (+ dest-start len))))

(defn parse-triple [range]
  (map parse-long (str/split range #"\s+")))

(defn parse-range [range]
  (apply expand-range (parse-triple range)))

(defn build-lookup [[dest-start source-start len]]
  #(if (<= source-start % (dec (+ source-start len)))
     (+ dest-start (- % source-start))))

(defn merge-lookup [acc look-up]
  #(or (look-up %) (acc %)))

#_(defn parse-mapping [ranges]
    (into {} (mapcat parse-range ranges)))

#_(defn parse-mappings [remaining-lines]
    (->> (map str/trim remaining-lines)
         (partition-by #{""})
         (remove #{'("")})
         (map (fn [[label & ranges]]
                [(parse-label label)
                 (into {} (parse-mapping ranges))]))
         (map second)
         (map (fn [hsh] #(get hsh %1 %1)))))

(defn parse-mapping [ranges]
  (->> (map parse-triple ranges)
       (map build-lookup)
       (reverse)
       (reduce merge-lookup)
       (merge-lookup identity)))

(defn parse-mappings [remaining-lines]
  (->> (map str/trim remaining-lines)
       (partition-by #{""})
       (remove #{'("")})
       (map rest)
       (map parse-mapping)))

(defn part-1 [input]
  (let [[first-line & remaining-lines] (str/split-lines input)
        seeds    (parse-seeds first-line)
        mappings (parse-mappings remaining-lines)]
    (reduce min (reduce (fn [ids mapping] (map mapping ids)) seeds mappings))))

#_(defn parse-seed-ranges [seed-line]
    (->> (str/split seed-line #"\s+")
         (map parse-long)
         (remove nil?)
         (partition 2)
         (mapcat (fn [[start len]] (range start (+ start len))))))

(defn check-ranges [pairs]
  (fn [x]
    (some (fn [[start len]] (<= start x (dec (+ start len)))) pairs)))

(defn seed-checker [seed-line]
  (->> (str/split seed-line #"\s+")
       (map parse-long)
       (remove nil?)
       (partition 2)
       (check-ranges)))

(defn build-rev-lookup [[dest-start source-start len]]
  #(if (<= dest-start % (dec (+ dest-start len)))
     (+ source-start (- % dest-start))))

(defn parse-reverse-mapping [ranges]
  (->> (map parse-triple ranges)
       (map build-rev-lookup)
       (reverse)
       (reduce merge-lookup)
       (merge-lookup identity)))

(defn parse-reverse-mappings [remaining-lines]
  (->> (map str/trim remaining-lines)
       (partition-by #{""})
       (remove #{'("")})
       (map rest)
       (map parse-reverse-mapping)))

(defn part-2 [input]
  (let [[first-line & remaining-lines] (str/split-lines input)
        initial?     (seed-checker first-line)
        mappings     (parse-reverse-mappings remaining-lines)
        map-to-start #(reduce (fn [id rev-map] (rev-map id)) % (reverse mappings))]
    (->> (range)
         (filter (comp initial? map-to-start))
         (first))))

(comment

  (parse-mappings (rest (str/split-lines input)))

  (def test-mapping (parse-mapping ["1 3 2" "10 6 2"]))
  (def test-reverse-mapping (parse-reverse-mapping ["1 3 2" "10 6 2"]))

  (map test-mapping (rest (range 14)))
  (map test-reverse-mapping (rest (range 14)))

  (map test-mapping (map test-reverse-mapping (rest (range 14))))
  (map test-reverse-mapping (map test-mapping (rest (range 14)))))

(part-1 example)
(part-1 input)

(comment
  ;; still super slow!
  (part-2 example)
  (part-2 input))
