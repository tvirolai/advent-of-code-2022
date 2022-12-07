(ns advent-of-code-2022.day07
  (:require [clojure.string :as s]))

(def input
  "resources/day07_test.txt")

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

(defn parse-dirs [rows curr res dir-stack]
  (let [{:keys [type cmd arg name size] :as curr-row} (first rows)
        curr-path (s/join dir-stack "/")]
    (println dir-stack)
    (println curr)
    (cond
      (empty? rows) (conj res curr)
      (= :directory type) (recur (rest rows)
                                 (update curr :subdirs conj (str curr-path (when-not (= "/" curr-path)
                                                                             "/") name))
                                 res
                                 dir-stack)
      (= :file type) (recur (rest rows)
                            (update curr :size + size)
                            res
                            dir-stack)
      (= :command type) (cond
                          (= ".." arg) (recur (rest rows) curr res (pop dir-stack))
                          (= "cd" cmd) (recur (rest rows) curr res (conj dir-stack arg))
                          (= "ls" cmd) (recur (rest rows) curr res dir-stack)
                          :else (recur (rest rows) (->Directory (str dir-stack "/" arg) 0 []) res dir-stack))
      #_(if (or (#{nil ".."} arg) (= "ls" cmd))

          (recur (rest rows) curr res)
          (recur (rest rows)
                 (->Directory arg 0 [])
                 (if (nil? curr)
                   res
                   (conj res curr)))))))

(defn total-size
  "Takes a sequence of directories and a single dir. Returns the dir
  with a total size added."
  [dirs dir-name]
  (let [{:keys [size subdirs]} (->> dirs
                                    (filter #(= dir-name (:name %)))
                                    first)]
    (if (empty? subdirs)
      size
      (reduce + size (map (partial total-size dirs) subdirs)))))

(defn part-01 []
  (let [data (parse-input input)
        dirs (parse-dirs data (->Directory "/" 0 []) [] [])
        dirs-with-sizes (for [{:keys [name] :as d} dirs]
                          (assoc d :total-size (total-size dirs name)))]
    dirs
    #_(reduce (fn [acc {:keys [total-size]}]
                (if (<= total-size 100000)
                  (+ acc total-size)
                  acc))
              0
              dirs-with-sizes)))
