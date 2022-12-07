(ns advent-of-code-2022.day07
  (:require [clojure.string :as s]))

(def input
  "resources/day07.txt")

(defrecord Command [type cmd arg])

(defrecord File [type size name])

(defrecord Dir [type name])

(defn parse-input [file]
  (let [rows (->> #"\n"
                  (s/split (slurp file))
                  (map #(s/split % #" ")))]
    (for [[fs md tl] rows]
      (condp = fs
        "$" (if (nil? tl)
              (->Command :command md nil)
              (->Command :command md tl))
        "dir" (->Dir :directory md)
        (->File :file (Integer/parseInt fs) md)))))

(defrecord Directory [name size subdirs])

(defn- parse-qualified-dirname [name dir-stack]
  (s/replace (str (s/join "/" dir-stack) "/" name)
             "//" "/"))

(defn parse-dirs [rows curr res dir-stack]
  (let [{:keys [type cmd arg name size]} (first rows)]
    (cond
      (empty? rows) (conj res curr)
      (= :directory type) (recur (rest rows)
                                 (update curr :subdirs conj (parse-qualified-dirname name dir-stack))
                                 res
                                 dir-stack)
      (= :file type) (recur (rest rows)
                            (update curr :size + size)
                            res
                            dir-stack)
      (= :command type) (cond
                          (= ".." arg) (recur (rest rows) curr res (pop dir-stack))
                          (= "ls" cmd) (recur (rest rows) curr res dir-stack)
                          (= "cd" cmd) (recur (rest rows) (->Directory (parse-qualified-dirname arg dir-stack) 0 [])
                                              (conj res curr)
                                              (conj dir-stack arg))))))

(defn total-size
  "Takes a sequence of directories and a single dir. Returns the dir
  with a total size added."
  [dirs dir-name]
  (let [{:keys [size subdirs] :as d} (->> dirs
                                          (filter #(= dir-name (:name %)))
                                          first)]
    (if (empty? subdirs)
      size
      (reduce + size (map (partial total-size dirs) subdirs)))))

(defn part-01 []
  (let [data (parse-input input)
        dirs (rest (parse-dirs data (->Directory "/" 0 []) [] []))
        dirs-with-sizes (for [{:keys [name] :as d} dirs]
                          (assoc d :total-size (total-size dirs name)))]
    (reduce (fn [acc {:keys [total-size]}]
              (if (<= total-size 100000)
                (+ acc total-size)
                acc))
            0
            dirs-with-sizes)))

(defn part-02 []
  (let [total-space 70000000
        space-needed 30000000
        data (parse-input input)
        dirs (rest (parse-dirs data (->Directory "/" 0 []) [] []))
        dirs-with-sizes (for [{:keys [name] :as d} dirs]
                          (assoc d :total-size (total-size dirs name)))
        used-size (:total-size (first (filter #(= "/" (:name %)) dirs-with-sizes)))
        unused-space (- total-space used-size)]
    (->> dirs-with-sizes
         (map :total-size)
         (filter #(>= (+ % unused-space) space-needed))
         sort
         first)))
