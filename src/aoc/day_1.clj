(ns aoc.day-1
  (:require [clojure.java.io :as io]))

(def input
  ;; This small data loading snippet is from @plexus
  (->> (io/resource "day1.txt")
       io/reader
       line-seq
       (mapv #(Long/parseLong %))))


;; Part 1
(-> (for [i (range 0 (count input))
          j (range (inc i) (count input))
          :let [x (input i)
                y (input j)]
          :when (= 2020 (+ x y))]
      (* x y))
    first)

;; Part 2
(-> (for [i (range 0 (count input))
          j (range (inc i) (count input))
          k (range (inc j) (count input))
          :let [x (input i)
                y (input j)
                z (input k)]
          :when (= 2020 (+ x y z))]
      (* x y z))
    first)

;; --- below is the optimized version, using a set lookup for the last step

(def input-set (set input))

;; Part 1
(-> (for [i (range 0 (count input))
          :let [x (input i)
                y (- 2020 x)]
          :when (contains? input-set y)]
      (* x y))
    first)

;; Part 2
(-> (for [i (range 0 (count input))
          j (range (inc i) (count input))
          :let [x (input i)
                y (input j)
                z (- 2020 x y)]
          :when (contains? input-set z)]
      (* x y z))
    first)
