(ns advent-of-code-2022.day04
  (:require [clojure.string :as s]
            [clojure.set :as cset]))

(defrecord Rule [start end length])

(def input
  (->> #"\n"
       (s/split (slurp "resources/day04.txt"))
       (reduce (fn [acc val]
                 (let [row (s/split val #",")
                       rule (mapv (fn [rang]
                                    (let [[hd tl] (mapv #(Integer/parseInt %) (s/split rang #"-"))]
                                      (->Rule hd
                                              tl
                                              (inc (- tl hd))))) row)]
                   (conj acc rule)))
               [])))

(defn total-overlap? [ranges]
  (let [[short long] (vec (sort-by :length ranges))]
    (and (<= (:start long)
             (:start short))
         (>= (:end long)
             (:end short)))))

(defn any-overlap? [[hd tl]]
  (seq (cset/intersection (set (range (:start hd)
                                      (inc (:end hd))))
                          (set (range (:start tl)
                                      (inc (:end tl)))))))

(defn part-01 []
  (count (filter total-overlap? input)))

(defn part-02 []
  (count (filter any-overlap? input)))
