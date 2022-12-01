(ns advent-of-code-2022.day01
  (:require [clojure.string :as s]))

(def input
  (let [lines (s/split (slurp "./resources/day01.txt") #"\n")]
    (->> lines
         (partition-by empty?)
         (remove (partial = '("")))
         (map (partial map read-string)))))

(defn day01 []
  (->> input
       (map (partial reduce +))
       (apply max)))

(defn day02 []
  (->> input
       (map (partial reduce +))
       sort
       (take-last 3)
       (reduce +)))
