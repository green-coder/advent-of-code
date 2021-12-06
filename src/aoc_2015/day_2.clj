(ns aoc-2015.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (slurp (io/resource "2015/day2.txt")))

;; Part 1
(transduce (map (fn [line]
                  (let [[a b c] (->> (re-seq #"\d+" line)
                                     (map parse-long)
                                     sort)]
                    (+ (* 2 a b)
                       (* 2 b c)
                       (* 2 c a)
                       (* a b)))))
           +
           (str/split-lines input))
; => 1588178

;; Part 2
(transduce (map (fn [line]
                  (let [[a b c] (->> (re-seq #"\d+" line)
                                     (map parse-long)
                                     sort)]
                    (+ a a b b
                       (* a b c)))))
           +
           (str/split-lines input))
; => 3783758
