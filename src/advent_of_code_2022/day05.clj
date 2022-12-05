(ns advent-of-code-2022.day05
  (:require [clojure.string :as s]))

(def test-input
  "resources/day05.txt")

(defn- initialize-stacks [state]
  (let [[stacks & rest] (reverse state)
        sts (apply merge (remove nil? (map-indexed (fn [i v]
                                                     (when (not= v \space)
                                                       (assoc {} i (Character/getNumericValue v))))
                                                   stacks)))
        insts (partition 2 (flatten (for [line rest]
                                      (remove nil? (map-indexed (fn [i v]
                                                                  (when (Character/isLetter v)
                                                                    [i v]))
                                                                line)))))]
    (reduce (fn [acc [i v]]
              (update acc (sts i) conj v))
            {}
            insts)))

(defrecord Instr [amount from to])

(defn parse-instr [instr]
  (let [[amount from to] (map #(Integer/parseInt %) (re-seq #"\d+" instr))]
    (->Instr amount from to)))

(defn read-input []
  (let [data (s/split (slurp test-input) #"\n")
        [state _ instr] (partition-by empty? data)]
    [(initialize-stacks state) (map parse-instr instr)]))

(defn next-state [stacks {:keys [amount from to]}]
  (if (zero? amount)
    stacks
    (let [source (stacks from)
          dest (stacks to)
          sts (-> stacks
                  (assoc from (pop source))
                  (assoc to (conj dest (peek source))))]
      (recur sts (->Instr (dec amount) from to)))))

(defn state->res [state]
  (->> state
       (into (sorted-map))
       vals
       (map first)
       (apply str)))

(defn part-01 []
  (let [[stacks instrs] (read-input)
        end-state (reduce next-state stacks instrs)]
    (state->res end-state)))

(defn next-state-02 [stacks {:keys [amount from to]}]
  (let [source (stacks from)
        dest (stacks to)]
    (-> stacks
        (assoc from (drop amount source))
        (assoc to (concat (take amount source) dest)))))

(defn part-02 []
  (let [[stacks instrs] (read-input)
        end-state (reduce next-state-02 stacks instrs)]
    (state->res end-state)))
