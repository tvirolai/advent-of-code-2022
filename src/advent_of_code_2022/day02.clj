(ns advent-of-code-2022.day02
  (:require [clojure.string :as s]))

(def test-input
  "resources/day02_test.txt")

(def input
  "resources/day02.txt")

(defn parse-input [filename]
  (for [line (s/split (slurp filename) #"\n")]
    (mapv keyword (s/split line #" "))))

(def tool-rank
  [:rock :paper :scissors])

(def tools
  {:A :rock
   :B :paper
   :C :scissors
   :X :rock
   :Y :paper
   :Z :scissors})

(defn result [[opponent you]]
  (let [opponent-tool (opponent tools)
        your-tool     (you tools)
        opponent-rank (inc (.indexOf tool-rank opponent-tool))
        your-rank     (inc (.indexOf tool-rank your-tool))]
    (cond
      (= opponent-tool your-tool)   :draw
      (and (= :scissors opponent-tool)
           (= :rock your-tool))     :win
      (and (= :scissors your-tool)
           (= :rock opponent-tool)) :lose
      (> opponent-rank your-rank)   :lose
      (< opponent-rank your-rank)   :win
      :else                         :win)))

(defn get-shape-score [tool]
  (condp = tool
    :rock 1
    :paper 2
    :scissors 3))

(defn get-result-score [result]
  (condp = result
    :lose 0
    :draw 3
    :win 6))

(defn score [round]
  (let [[opponent you] round
        your-tool (you tools)
        result (result round)
        result-score (get-result-score result)
        shape-score (get-shape-score your-tool)]
    (+ result-score shape-score)))

(defn part-01 []
  (let [data (parse-input input)]
    (->> data
         (map score)
         (reduce +))))

(defn get-outcome [outcome-code]
  (condp = outcome-code
    :X :lose
    :Y :draw
    :Z :win))

(defn result-02 [[opponent outcome-code]]
  (let [opponent-tool (opponent tools)
        opponent-index (.indexOf tool-rank opponent-tool)
        outcome (get-outcome outcome-code)]
    (condp = outcome
      :lose (if (zero? opponent-index)
              :scissors
              (nth tool-rank (dec opponent-index)))
      :draw opponent-tool
      :win (if (= :scissors opponent-tool)
             :rock
             (nth tool-rank (inc opponent-index))))))

(defn score-02 [round]
  (let [shape (result-02 round)
        result (get-outcome (last round))]
    (+ (get-shape-score shape)
       (get-result-score result))))

(defn part-02 []
  (let [data (parse-input input)]
    (->> data
         (map score-02)
         (reduce +))))
