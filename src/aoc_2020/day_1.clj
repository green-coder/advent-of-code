(ns aoc-2020.day-1
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [aoc.util :refer :all]))

(def input
  ;; This small data loading snippet is from @plexus
  (->> (io/resource "2020/day1.txt")
       io/reader
       line-seq
       (mapv #(Long/parseLong %))))


;; Part 1
(first-for [i (range 0 (count input))
            j (range (inc i) (count input))
            :let [x (input i)
                  y (input j)]
            :when (= 2020 (+ x y))]
  (* x y))

;; Part 2
(first-for [i (range 0 (count input))
            j (range (inc i) (count input))
            k (range (inc j) (count input))
            :let [x (input i)
                  y (input j)
                  z (input k)]
            :when (= 2020 (+ x y z))]
  (* x y z))

;; --- below is the optimized version, using a set lookup for the last step

(def input-set (set input))

;; Part 1
(first-for [i (range 0 (count input))
            :let [x (input i)
                  y (- 2020 x)]
            :when (contains? input-set y)]
  (* x y))

;; Part 2
(first-for [i (range 0 (count input))
            j (range (inc i) (count input))
            :let [x (input i)
                  y (input j)
                  z (- 2020 x y)]
            :when (contains? input-set z)]
  (* x y z))
