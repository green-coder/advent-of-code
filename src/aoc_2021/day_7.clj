(ns aoc-2021.day-7
  (:require [clojure.java.io :as io]
            [clojure.java.math :refer [abs]] ;; new in Clojure 1.11.0-alpha3
            [aoc.util :as u]))

(def input
  (->> (io/resource "2021/day7.txt")
       slurp
       (re-seq #"\d+")
       (map parse-long)))

;; Part 1 - brute force, super slow, don't do it at home.
(defn cost1 [n input]
  (transduce (map (fn [x] (abs (- x n)))) + input))

(->> (for [n (range (apply min input)
                    (apply max input))]
       [n (cost1 n input)])
     (u/min-by second)
     second)
; => 355592

;; Part 2 - brute force, super slow, don't do it at home.
(defn foobar [n]
  (/ (* n (inc n)) 2))

(defn cost2 [n input]
  (transduce (map (fn [x]
                    (foobar (abs (- x n)))))
             +
             input))

(->> (for [n (range (apply min input)
                    (apply max input))]
       [n (cost2 n input)])
     (u/min-by second)
     second)
; => 101618069
