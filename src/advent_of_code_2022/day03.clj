(ns advent-of-code-2022.day03
  (:require [clojure.string :as s]
            [clojure.set :as cset]))

(def input
  (s/split (slurp "resources/day03.txt") #"\n"))

(def priority-map
  (let [downcase (zipmap (range (int \a) (inc (int \z))) (range 1 27))
        upcase (zipmap (range (int \A) (inc (int \Z))) (range 27 53))]
    (merge downcase upcase)))

(defn get-priority [c]
  (get priority-map (int c)))

(defn get-shared-item [line]
  (let [pivot (/ (count line) 2)]
    (first (cset/intersection (set (subs line 0 pivot))
                              (set (subs line pivot))))))

(defn part-01 []
  (transduce (map (comp get-priority get-shared-item)) + input))

(defn get-shared-item-02 [lines]
  (->> lines
       (map set)
       (apply cset/intersection)
       first))

(defn part-02 []
  (transduce (comp (partition-all 3)
                   (map get-shared-item-02)
                   (map get-priority)) + input))
