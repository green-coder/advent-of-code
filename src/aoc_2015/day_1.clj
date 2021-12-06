(ns aoc-2015.day-1
  (:require [clojure.java.io :as io]))

(def input
  (slurp (io/resource "2015/day1.txt")))

;; Part 1
(transduce (map {\( 1 \) -1}) + input)
; => 138

;; Part 2
(reduce-kv (fn [floor index c]
             (let [floor (case c
                           \( (inc floor)
                           \) (dec floor))]
               (if (neg? floor)
                 (reduced (inc index))
                 floor)))
           0
           (vec input))
; => 1771
