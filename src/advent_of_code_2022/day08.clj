(ns advent-of-code-2022.day08
  (:require [clojure.string :as s]))

(def input
  "resources/day08.txt")

(defn parse-input [filename]
  (let [data (s/split (slurp filename) #"\n")]
    (vec (for [line data]
           (mapv #(Character/digit % 10) line)))))

(defn col->vec [grid col-number]
  (reduce (fn [acc row]
            (conj acc (nth row col-number)))
          []
          grid))

(defn visible? [grid {:keys [x y]}]
  (let [row (nth grid y)
        col (col->vec grid x)
        point-val (nth (nth grid y) x)
        directions-row [(subvec row 0 x) (subvec row (inc x))]
        directions-col [(subvec col 0 y) (subvec col (inc y))]
        directions (concat directions-row directions-col)]
    (some true? (map (fn [dir]
                       (if (empty? dir)
                         true
                         (every? #(> point-val %) dir))) directions))))

(defrecord Point [x y])

(defn part-01 []
  (let [grid (parse-input input)]
    (count (for [x (range (count (first grid)))
                 y (range (count grid))
                 :when (true? (visible? grid (->Point x y)))]
             true))))

(defn take-until
  "This again: https://groups.google.com/g/clojure-dev/c/NaAuBz6SpkY"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest s)))))))

(defn scenic-score [grid {:keys [x y]}]
  (let [row (nth grid y)
        col (col->vec grid x)
        point-val (nth (nth grid y) x)
        directions-row [(reverse (subvec row 0 x)) (subvec row (inc x))]
        directions-col [(reverse (subvec col 0 y)) (subvec col (inc y))]
        scores (map (fn [dir-vec]
                      (count (take-until #(>= % point-val) dir-vec)))
                    (concat directions-row directions-col))]
    (apply * scores)))

(defn part-02 []
  (let [grid (parse-input input)]
    (apply max (for [x (range (count (first grid)))
                     y (range (count grid))]
                 (scenic-score grid (->Point x y))))))
