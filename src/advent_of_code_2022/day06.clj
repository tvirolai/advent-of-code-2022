(ns advent-of-code-2022.day06
  (:require [clojure.string :as s]))

(def input
  (slurp "resources/day06.txt"))

(defn solve [packet-length]
  (let [start-packet (->> input
                          (partition-all packet-length 1)
                          (map distinct)
                          (filter #(= packet-length (count %)))
                          first
                          (apply str))]
    (+ packet-length (s/index-of input start-packet))))

(defn part-01 []
  (solve 4))

(defn part-02 []
  (solve 14))
