(ns aoc.day-3
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "day3.txt")
       io/reader
       line-seq
       (mapv (fn [line]))))

;; Part 1
(->> input)

;; Part 2
(->> input)
