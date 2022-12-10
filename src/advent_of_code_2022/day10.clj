(ns advent-of-code-2022.day10
  (:require [clojure.string :as s]
            [clojure.set :as cset]))

(def input
  (for [line (s/split (slurp "resources/day10.txt") #"\n")
        :let [[hd tl] (s/split line #" ")]]
    [hd (if (nil? tl)
          tl
          (Integer/parseInt tl))]))

(def cycles-to-watch #{20 60 100 140 180 220})

(defn solve* [instr x-reg cycle values]
  (if (empty? instr)
    [x-reg values]
    (let [[command val] (first instr)]
      (if (= command "noop")
        (recur (rest instr)
               x-reg
               (inc cycle)
               (if (cycles-to-watch cycle)
                 (do
                   (println [cycle x-reg])
                   (conj values [cycle x-reg]))
                 values))
        (let [current-command-cycles (range cycle (+ 2 cycle))
              cycle-in-sequence (first (cset/intersection cycles-to-watch (set current-command-cycles)))]
          (recur (rest instr)
                 (+ x-reg val)
                 (+ cycle 2)
                 (if (number? cycle-in-sequence)
                   (do
                     (println "Täsä")
                     (println cycle)
                     (println cycle-in-sequence)
                     (println [x-reg val])
                     (conj values [cycle-in-sequence x-reg]))
                   values)))))))

(defrecord State [cycle reg-val instruction])

(defn cycle-states-for-instr [instr x-reg start-cycle]
  (let [[command amount] instr]
    (if (= "noop" command)
      (list
       (->State (inc start-cycle) x-reg instr))
      (for [i (range start-cycle (+ start-cycle 2))
            :let [reg-val (if (= i (inc start-cycle))
                            (+ amount x-reg)
                            x-reg)]]
        (->State (inc i) reg-val instr)))))

(defn solve [instr cycle x-reg states]
  (if (empty? instr)
    states
    (let [instruction (first instr)
          next-states (cycle-states-for-instr instruction x-reg cycle)]
      (recur (rest instr)
             (:cycle (last next-states))
             (:reg-val (last next-states))
             (concat states next-states)))))

(defn part-01
  "Ffs with this off-by-one..."
  []
  (let [states (solve input 0 1 [])
        right-states (map (fn [state]
                            (update state :cycle inc))
                          (filter (fn [{:keys [cycle]}]
                                    (contains? cycles-to-watch (inc cycle))) states))]
    (reduce (fn [acc {:keys [cycle reg-val]}]
              (+ acc (* cycle reg-val)))
            0
            right-states)))

(defn draw-row [row instr]
  (s/join (for [i (range 0 (count instr))
                :let [value (nth instr i)
                      {:keys [cycle reg-val instruction]} value
                      sprite-position (+ reg-val (* 40 row))
                      sprite (set (range (dec sprite-position) (+ 2 sprite-position)))]]
            (let [res (if (contains? sprite cycle)
                        "#"
                        ".")]
              res))))

(defn part-02 []
  (let [states (solve input 0 1 [])
        rows (partition 40 states)]
    (doseq [line (map-indexed draw-row rows)]
      (println line))))
