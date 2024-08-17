(ns day20
  (:require
   [clojure.string :as str]
   [utils :as u]))

(def example
  "broadcaster -> a, b, c
   %a -> b
   %b -> c
   %c -> inv
   &inv -> a")

<<<<<<< HEAD
(def example-2
  "broadcaster -> a
   %a -> inv, con
   &inv -> b
   %b -> con
   &con -> output")

(def input (slurp "20.txt"))

;; flip-flop - %
;; has on/off state, starts off
;; changes state on low pulse
;; off->on sends high
;; on->off sends low
;;
;; conjunction - &
;; remember last pulse for EACH connection, starts low
;; first - update memory for that connection
;; all high -> send low
;; otherwise -> send high
;;
;; broadcaster -> forwards each pulse to all connections
;;
;; button -> sends low pulse to broadcaster
;;
;; must wait until all pulses are sent before pushing button again
;;
;; pulses are processed in the order they are sent
;;
;; ... we will use false as low and true as high
;; flip -> when given false, apply not, send the new value
;; conj -> when all true, send false, else send true

(defn parse-line [l]
  (let [[df dests] (map str/trim (str/split l #"->"))
        t          (case (first df)
                     \% :flip
                     \& :conj
                     :none)
        nm         (if (= :none t)
                     df
                     (subs df 1))]
    {:type         t
     :name         nm
     :destinations (map str/trim (str/split dests #","))
     :state        nil}))

(defn- notify-connections [nodes]
  (reduce
   (fn [nodes {from :name :keys [destinations]}]
     (reduce
      (fn [nodes to]
        (if (= :conj (:type (nodes to)))
          (assoc-in nodes [to :state from] false)
          nodes))
      nodes
      destinations))
   nodes
   (vals nodes)))

(def ^:private init-pulse
  {:from "button" :to "broadcaster" :pulse false})

(defn- parse [s]
  {:pulses []
   :nodes  (notify-connections
            (reduce (fn [m x]
                      (assoc m (:name x) x))
                    {}
                    (map parse-line (u/lines s))))})

(defn send-pulses [from destinations pulse]
  (map (fn [d] {:from from :to d :pulse pulse})
       destinations))

(defn- react [{n :name t :type :keys [destinations state] :as node} {:keys [from pulse] :as p}]
  (case t
    :none [state (send-pulses n destinations pulse)]
    :flip [(if pulse
             state
             (not state))
           (when-not pulse
             (send-pulses n destinations (not state)))]
    :conj (let [state' (assoc state from pulse)]
            [state'
             (send-pulses n destinations
                          (if (every? val state')
                            false
                            true))])
    nil))

(defn- step [{cnts :counts :keys [pulses nodes]}]
  (if-not (seq pulses)
    {:pulses []
     :nodes  nodes
     :counts cnts}
    (let [p                      (first pulses)
          to                     (:to p)
          [new-state new-pulses] (react (get nodes to) p)]
      {:pulses (concat (rest pulses) new-pulses)
       :nodes  (assoc-in nodes [to :state] new-state)
       :counts (when cnts
                 (update cnts (:pulse p) inc))})))

(defn- count-pulses [state]
  (loop [pushes 0
         s      (assoc state :counts {true 0 false 0})]
    (if (seq (:pulses s))
      (recur pushes (step s))
      (if (= 1000 pushes)
        (:counts s)
        (recur (inc pushes)
               (assoc s :pulses [init-pulse]))))))

(defn part1 [s]
  (->> s
       parse
       count-pulses
       vals
       (reduce *)))

;; this is too slow
(defn- push-until-rx [state]
  (loop [pushes 0
         s      state]
    (if (< 1e20 pushes)
      ::infinity
      (if-let [p (first (:pulses s))]
        (do #_(when (= "rx" (:to p))
                (prn p (hash (:nodes s))))
         (if (= {:to "rx" :pulse false} (select-keys p [:to :pulse]))
           pushes
           (recur pushes (step s))))
        (recur (inc pushes)
               (assoc s :pulses [init-pulse]))))))

;; faster approach
;; work backwards from each input to rx, to calculate its cycle
;;
;; are there offsets until each cycle begins though?
;; what about interactions between these nodes?

(defn part2 [s]
  (->> s parse push-until-rx))

(comment
  {:a 3
   :b 9
   :c 2}
  (parse example)
  (parse input)

  (->> input
       parse
       :nodes
       vals
       (mapcat #(:destinations %))
       set
       sort)

  (-> example
      parse
      step)

  {:a 3
   :b 3}

  (part1 example)
  (part1 example-2)
  (part1 input)

  (part2 example)
  (part2 example-2)
  (part2 input)

  (prn (part2 input)))
