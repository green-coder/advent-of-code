(ns aoc-2021.day-7-fast
  (:require [clojure.java.io :as io]
            [clojure.java.math :refer [abs]] ;; new in Clojure 1.11.0-alpha3
            [aoc.util :as u]))

(def input
  (->> (io/resource "2021/day7.txt")
       slurp
       (re-seq #"\d+")
       (map parse-long)
       sort))

;; Part 1 - brute force, super slow, don't do it at home.
(defn cost1 [n input]
  (transduce (map (fn [x] (abs (- x n)))) + input))

;; The cost of the median position of the crabs
(-> (sort input)
    (nth (/ (count input) 2))
    (cost1 input))


;; Part 2 - brute force, super slow, don't do it at home.
(defn foobar [n]
  (/ (* n (inc n)) 2))

(defn cost2 [n input]
  (transduce (map (fn [x]
                    (foobar (abs (- x n)))))
             +
             input))

;; The cost of the average position of the crabs.
(-> (reduce + input)
    (quot (count input))
    (cost2 input))
; => 101618069

;; Credits for those awesome solutions go to:
;; - k24883 (Sebastian)
;; - MrMopi5002
