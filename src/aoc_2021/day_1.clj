(ns aoc-2021.day-1
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "2021/day1.txt")
       io/reader
       line-seq
       (mapv parse-long)))

;; Part 1
(->> input
     (partition 2 1)
     (map (fn [[x y]]
            (< x y)))
     (filter true?)
     count)
; => 1766

;; Part 2
(->> input
     (partition 3 1)
     (map (fn [[x y z]]
            (+ x y z)))
     (partition 2 1)
     (map (fn [[x y]]
            (< x y)))
     (filter true?)
     count)
; => 1797
