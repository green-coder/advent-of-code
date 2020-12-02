(ns aoc.day-2
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "day2.txt")
       io/reader
       line-seq
       (mapv (fn [line]
               (let [[_ min max letter password] (re-find #"(\d+)-(\d+) ([a-zA-Z]): ([a-zA-Z]+)" line)]
                 [(Long/parseLong min)
                  (Long/parseLong max)
                  (first letter)
                  password])))))

;; Part 1
(->> input
     (filter (fn [[min max letter password]]
               (<= min (count (filter #{letter} password)) max)))
     count)

;; Part 2
(->> input
     (filter (fn [[pos1 pos2 letter password]]
               (not= (= (nth password (dec pos1)) letter)
                     (= (nth password (dec pos2)) letter))))
     count)
