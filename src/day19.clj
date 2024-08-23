(ns day19
  (:require [clojure.string :as str]))

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
          k   (keyword k)
          cmp (case (first c) \< < \= = \> >)
          v   (parse-long v)]
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

(defn pred->range [r]
  (if-not r
    ["x" {:min 1 :max max-range}]
    (let [c (second r)
          n (parse-long (subs r 2))]
      [(first r)
       (case c
         \> {:min (inc n) :max max-range}
         \< {:min 1 :max (dec n)}
         \= {:min n :max n})])))

(defn invert-constraint [r]
  ;; currently gets a range, not a raw constraint
  (when r
    (let [c (second r)
          n (parse-long (subs r 2))]
      [(first r)
       (case c
         \> [{:min 1 :max n}]
         \< [{:min n :max max-range}]
         \= [{:min 1 :max (dec n)}
             {:min (inc n) :max max-range}])])))

(defn- union-constraints [cs]
  (->> cs
       (remove nil?)
       (sort-by :min)
       (reduce
        (fn [[acc {curr-mn :min curr-mx :max :as curr}] {mn :min mx :max :as nxt}]
          (if (<= mn curr-mx)
            [acc {:min curr-mn :max (max curr-mx mx)}]
            [(conj acc curr) nxt]))
        [[] {:min 0 :max 0}])
       (apply conj)
       rest
       (remove nil?)
       vec))

(defn- intersect-constraints [cs]
  (reduce (fn [acc x]
            {:min (max (:min acc) (:min x))
             :max (min (:max acc) (:max x))})
          {:min 1 :max max-range}
          cs))

(defn- intersect [xmasses]
  (zipmap "xmas"
          (for [k "xmas"]
            (intersect-constraints (keep #(% k) xmasses)))))

(comment
  (union-constraints [{:min 1 :max 10}
                      {:min 8 :max 12}
                      {:min 14 :max 14}
                      {:min 20 :max 300}
                      {:min 30 :max 40}])

  (union-constraints [{:min 1 :max 10}
                      {:min 8 :max 12}
                      {:min 14 :max 14}
                      {:min 20 :max 300}
                      {:min 30 :max 40}])

  (intersect-constraints [{:min 1 :max 20}
                          {:min 15 :max 30}])

  (intersect [{\x {:min 1 :max 20}}
              {\a {:min 15 :max 30}}]))

(defn merge-constraints [xs ys]
  (union-constraints
   (for [{mnx :min mxx :max} (or xs [{:min 1 :max max-range}])
         {mny :min mxy :max} (or ys [{:min 1 :max max-range}])
         :when (and (<= mnx mxy)
                    (<= mny mxx))]
     {:min (max mnx mny)
      :max (min mxx mxy)})))

(defn- count-range [{mn :min mx :max}]
  (max 0 (inc (- mx mn))))

(defn- count-ranges [rs]
  (if-not rs
    max-range
    (reduce + (map count-range rs))))

(defn- count-cs [c->cs]
  (->> (map c->cs "xmas")
       (map count-ranges)
       (reduce *)))

(defn ranges->dispatches [rs]
  (mapv (comp vec rest rest)
        (rest
         (reductions
          (fn [[cs last-tc _ _] {:keys [pred target]}]
            (let [last-inv   (invert-constraint last-tc)
                  curr-range (pred->range pred)
                  prior      (when last-inv
                               (update cs
                                       (first last-inv)
                                       #(merge-constraints %
                                                           (second last-inv))))
                  curr-cs    (update prior
                                     (first curr-range)
                                     #(merge-constraints %
                                                         [(second curr-range)]))]
              [prior
               pred
               curr-cs
               target]))
          ;; constraints, target
          [{} nil nil]
          rs))))

(defn step [[target->constraints constraints->targets] rules]
  (merge-with + target->constraints
              (into {}
                    (for [[cs target] constraints->targets]
                      nil))))

(defn flappy
  "Flip and apply - cycles the last argument to the first position."
  [f & xs]
  (apply f (last xs) (butlast xs)))

(defn- infer-sinks
  "Given a map from every entry point to all the rules that dispatch from it,
  return a map of the final destinations to the combined constraints of all
  the routes which lead to it."
  [in->dispatches]
  (loop [target->routes {}
         inbound        [["in" nil]]]
    (if-not (seq inbound)
      target->routes
      (recur
       (reduce
        (fn [acc [target cs]]
          (merge-with concat
                      acc
                      ;; TODO we only need to register stuff going to A really
                      nil))
        target->routes
        inbound)
       ;; next outbound
       nil))))

(defn routes->disjoint
  "Given a seq of constraint corresponding to all the routes somewhere, break
  them up into disjoint sets so that we can calculate the viable combinations."
  [routes-cs])

(defn part-2 [i]
  ;; we are calculating how many combinations satisfy *all* the rules
  ;; that's obviously not what we want (as there are none)
  ;; ... oh
  ;; we need to figure out the chains of rules which need to be combined
  ;; and sum over all those chains
  ;; remembering to take the complement of the rules we must pass over
  #_(let [[workflows _] (str/split i #"\n\n")]
      (->> (str/split-lines workflows)
           (map parse-workflow)
           (map :rules)
           (mapcat (partial keep :pred))
           (group-by first)
           (vals)
           (mapv #(count-range (collapse-ranges (map pred->range %))))
           prn
           (reduce *))))

(comment
  (part-1 example)
  (part-1 input)

  (part-2 example)
  (part-2 input))
