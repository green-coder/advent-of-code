(ns aoc.day-3
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "day3.txt")
       io/reader
       line-seq
       (mapv (fn [line] line))))

(def map-width (-> input (nth 0) count))
(def map-height (count input))

;; Part 1
(->> (for [y (range (count input))
           :let [x (mod (* y 3) map-width)]]
       (get-in input [y x]))
     (filter #{\#})
     count)

;; Part 2
(->> (for [[slope-x slope-y] [[1 1]
                              [3 1]
                              [5 1]
                              [7 1]
                              [1 2]]]
       (->> (iterate (fn [[x y]]
                       [(mod (+ x slope-x) map-width) (+ y slope-y)])
                     [0 0])
            (take-while (fn [[x y]]
                          (< y map-height)))
            (map (fn [[x y]]
                   (get-in input [y x])))
            (filter #{\#})
            count))
     (apply *))
