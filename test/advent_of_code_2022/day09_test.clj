(ns advent-of-code-2022.day09-test
  (:require [advent-of-code-2022.day09 :refer [->Rule
                                               ->Position
                                               next-tail-position]]
            [clojure.test :refer [deftest is]]))

(deftest tail-moving-test
  (is (= (->Position 0 0) (next-tail-position (->Position 0 0)
                                              (->Position 0 0))))
  (is (= (->Position 0 0) (next-tail-position (->Position 0 1)
                                              (->Position 0 0))))
  (is (= (->Position 0 0) (next-tail-position (->Position 0 -1)
                                              (->Position 0 0))))
  (is (= (->Position 0 0) (next-tail-position (->Position 1 0)
                                              (->Position 0 0))))
  (is (= (->Position 0 0) (next-tail-position (->Position 1 1)
                                              (->Position 0 0))))
  (is (= (->Position 1 1) (next-tail-position (->Position 1 2)
                                              (->Position 0 0))))
  (is (= (->Position -1 -1) (next-tail-position (->Position -1 -2)
                                                (->Position 0 0))))
  (is (= (->Position 3 2) (next-tail-position (->Position 4 2)
                                              (->Position 2 2))))
  (is (= (->Position 3 2) (next-tail-position (->Position 4 2)
                                              (->Position 2 2))))
  (is (= (->Position 1 1) (next-tail-position (->Position 2 2)
                                              (->Position 0 0))))
  (is (= (->Position 4 1) (next-tail-position (->Position 4 0)
                                              (->Position 3 2))))
  (is (= (->Position 4 2) (next-tail-position (->Position 4 3)
                                              (->Position 4 1))))
  (is (= (->Position 3 1) (next-tail-position (->Position 4 2)
                                              (->Position 3 1))))
  (is (= (->Position 2 1) (next-tail-position (->Position 3 1)
                                              (->Position 1 0)))))
