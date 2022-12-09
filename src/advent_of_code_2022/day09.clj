(ns advent-of-code-2022.day09
  (:require [clojure.string :as s]))

(defrecord Rule [dir amount])

(def input
  (for [line (-> "resources/day09.txt" slurp (s/split #"\n"))
        :let [[d a] (s/split line #" ")]]
    (->Rule (keyword d) (Integer/parseInt a))))

(defrecord Position [x y])

(defn move-position
  "Takes a direction and a head position, returns the updated position."
  [dir point]
  (condp = dir
    :U (update point :y inc)
    :D (update point :y dec)
    :L (update point :x dec)
    :R (update point :x inc)))

(defn next-tail-position
  "Takes a head and a tail, returns the updated tail position."
  [{:keys [x y] :as head} tail]
  (let [tx (:x tail)
        ty (:y tail)
        manhattan-distance (+ (- (Math/max x tx)
                                 (Math/min x tx))
                              (- (Math/max y ty)
                                 (Math/min y ty)))
        diagonal (and (not= x tx)
                      (not= y ty))]
    (cond
      (= head tail) tail
      (and diagonal
           (or (= 3 manhattan-distance)
               (= 4 manhattan-distance))) (cond
                                            (and (> x tx)
                                                 (> y ty)) (->> tail
                                                                (move-position :R)
                                                                (move-position :U))
                                            (and (< x tx)
                                                 (> y ty)) (->> tail
                                                                (move-position :L)
                                                                (move-position :U))
                                            (and (> x tx)
                                                 (< y ty)) (->> tail
                                                                (move-position :R)
                                                                (move-position :D))
                                            (and (< x tx)
                                                 (< y ty)) (->> tail
                                                                (move-position :L)
                                                                (move-position :D)))
      (and (false? diagonal)
           (= 2 manhattan-distance)) (cond
                                       (= x tx) (if (> y ty)
                                                  (move-position :U tail)
                                                  (move-position :D tail))
                                       (= y ty) (if (> x tx)
                                                  (move-position :R tail)
                                                  (move-position :L tail)))
      :else tail)))

(defn solve [rules head tail tail-positions]
  (if (empty? rules)
    tail-positions
    (let [{:keys [dir amount] :as rule} (first rules)
          new-head (move-position dir head)
          new-tail (next-tail-position new-head tail)]
      (recur (if (= 1 amount)
               (rest rules)
               (conj (rest rules) (update rule :amount dec)))
             new-head
             new-tail
             (conj tail-positions new-tail)))))

(defn part-01 []
  (let [visited-points (solve input
                              (->Position 0 0)
                              (->Position 0 0)
                              [(->Position 0 0)])]
    (count (set visited-points))))

(defn solve-02 [rules knots tail-positions]
  (if (empty? rules)
    tail-positions
    (let [{:keys [dir amount] :as rule} (first rules)
          new-head (move-position dir (first knots))
          next-rope (loop [rp (rest knots)
                           new-rope [new-head]]
                      (if (empty? rp)
                        new-rope
                        (let [hd (last new-rope)
                              tl (first rp)]
                          (recur (rest rp)
                                 (conj new-rope (next-tail-position hd tl))))))]
      (recur (if (= 1 amount)
               (rest rules)
               (conj (rest rules) (update rule :amount dec)))
             next-rope
             (conj tail-positions (last next-rope))))))

(defn part-02 []
  (let [positions (solve-02 input
                            (repeat 10 (->Position 0 0))
                            [(->Position 0 0)])]
    (count (set positions))))
