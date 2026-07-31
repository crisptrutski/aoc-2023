(ns day19
  (:require
   [clojure.string :as str]
   [utils :as u]))

(def example
  "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")

(def input (try (slurp "19.txt") (catch Exception _)))

(def max-range 4000)

(defn compile-pred [c]
  (if (nil? c)
    (constantly true)
    (let [[_ k c v] (re-matches #"(\w)(.)(\d+)" c)
          k         (keyword k)
          cmp       (case (first c) \< < \= = \> >)
          v         (parse-long v)]
      (fn [p]
        (cmp (get p k) v)))))

(defn- parse-rule [r]
  (let [[condition target] (str/split r #":")]
    (if-not target
      {:pred   nil
       :target condition}
      {:pred   condition
       :target target})))

(defn- parse-workflow [w]
  (let [[_ nm rules] (re-matches #"(\w+)\{([^}]+)\}" w)]
    {:name  nm
     :rules (map parse-rule (str/split rules #","))}))

(defn- parse-part [s]
  (let [[_ x m a s] (re-matches #"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}" s)]
    (update-vals {:x x :m m :a a :s s} parse-long)))

(defn- sum-part [p]
  (reduce + (vals p)))

(defn- parse-input [i]
  (let [[workflows parts] (str/split i #"\n\n")]
    {:workflows (into {}
                      (comp (map parse-workflow)
                            (map (juxt :name :rules)))
                      (str/split-lines workflows))
     :parts     (mapv parse-part (str/split-lines parts))}))

(defn- apply-rule [part {:keys [pred target]}]
  (when ((compile-pred pred) part)
    target))

(defn process [workflows part]
  (loop [workflow "in"]
    (if-let [rules (get workflows workflow)]
      (recur (first (keep (partial apply-rule part) rules)))
      workflow)))

(defn part-1 [i]
  (let [{:keys [workflows parts]} (parse-input i)]
    (->> parts
         (map (juxt (partial process workflows) identity))
         (filter (comp #{"A"} first))
         (map (comp sum-part second))
         (reduce +))))

;;;

(def ^:private full-range {:min 1 :max max-range})

(def ^:private full-cube (zipmap [:x :m :a :s] (repeat full-range)))

(defn pred->range [r]
  (when r
    (let [c (second r)
          n (parse-long (subs r 2))]
      (case c
        \> {:min (inc n) :max max-range}
        \< {:min 1 :max (dec n)}
        \= {:min n :max n}))))

(defn invert-constraint [r]
  (when r
    (let [c (second r)
          n (parse-long (subs r 2))]
      (case c
        \> [{:min 1 :max n}]
        \< [{:min n :max max-range}]
        \= [{:min 1 :max (dec n)}
            {:min (inc n) :max max-range}]))))

(defn merge-range [r1 r2]
  (cond (not r1) r2
        (not r2) r1
        :else
        {:min (max (:min r1) (:min r2))
         :max (min (:max r1) (:max r2))}))

(defn merge-constraints [cube k r]
  (if-not r
    cube
    (update cube k (partial merge-range r))))

(defn- steps-from [node->rules node cube]
  (let [rules (node->rules node)]
    (second
     (reduce
      (fn [[cubes dispatches] {:keys [k target valid invalid]}]
        [(mapcat (fn [i] (map #(merge-constraints % k i) cubes)) invalid)
         (into dispatches (map (fn [cube] [target (merge-constraints cube k valid)]) cubes))])
      [[cube] []]
      rules))))

(defn- terminal-cubes
  "Given a network, return a list of all the configuration hypercubes that reach A from in."
  [node->rules]
  (loop [heads [["in" full-cube]]
         tails []
         seen #{}]
    (if (empty? heads)
      tails
      (let [nxt (remove seen (mapcat (partial apply steps-from node->rules) heads))]
        (recur
         (remove (comp #{"A"} first) nxt)
         (into tails (map second (filter (comp #{"A"} first) nxt)))
         (into seen nxt))))))

(defn parse-rule-pred [{:keys [pred] :as m}]
  (if-not pred
    m
    (assoc m
           :k (keyword (str (first pred)))
           :valid (pred->range pred)
           :invalid (invert-constraint pred))))

(defn parse-preds [node->rules]
  (update-vals node->rules #(map parse-rule-pred %)))

(defn- count-range [{mn :min mx :max}]
  (if-not mn
    0
    (max 0 (inc (- mx mn)))))

(defn- count-cs [c->cs]
  (->> (map c->cs (map (comp keyword str) "xmas"))
       (map count-range)
       (reduce *)))

(defn part-2 [i]
  (let [[workflows _] (str/split i #"\n\n")]
    (->> (str/split-lines workflows)
         (map parse-workflow)
         (into {} (map (juxt :name :rules)))
         parse-preds
         terminal-cubes
         (map count-cs)
         (reduce +)
         u/pp))
  true)

(comment
  (part-1 example)
  (part-1 input)

  (part-2 example)
  (part-2 input))
