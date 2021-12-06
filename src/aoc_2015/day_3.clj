(ns aoc-2015.day-3
  (:require [clojure.java.io :as io]))

(def input
  (slurp (io/resource "2015/day3.txt")))

(defn visited-houses [commands]
  (-> (reduce (fn [{:keys [output x y]} c]
                (let [[dx dy] ({\^ [0 1]
                                \v [0 -1]
                                \< [-1 0]
                                \> [1 0]} c)
                      x (+ x dx)
                      y (+ y dy)]
                  {:output (conj output [x y])
                   :x x
                   :y y}))
              {:output [[0 0]]
               :x 0
               :y 0}
              commands)
      :output))

;; Part 1
(-> input
    visited-houses
    frequencies
    keys
    count)
; => 2572

;; Part 2
(let [commands-for-santa       (take-nth 2 input)
      commands-for-robot-santa (take-nth 2 (next input))]
  (-> (visited-houses commands-for-santa)
      (into (visited-houses commands-for-robot-santa))
      frequencies
      keys
      count))
; => 2631
