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
  (let [pivot (/ (count line) 2)
        hd (subs line 0 pivot)
        tl (subs line pivot)]
    (first (cset/intersection (set hd) (set tl)))))

(defn part-01 []
  (->> input
       (map get-shared-item)
       (map get-priority)
       (reduce +)))

(defn get-shared-item-02 [lines]
  (->> lines
       (map set)
       (apply cset/intersection)
       first))

(defn part-02 []
  (->> input
       (partition 3)
       (map get-shared-item-02)
       (map get-priority)
       (reduce +)))
