(ns aoc-2021.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-binary [s]
  (edn/read-string (str "2r" (str/triml s))))

(def input
  (->> (io/resource "2021/day3.txt")
       io/reader
       line-seq))

;; Part 1
(let [s (for [index (-> input first count range)
              :let [{zeroes \0
                     ones   \1} (->> input
                                     (map (fn [bin] (nth bin index)))
                                     frequencies)]]
          (if (< zeroes ones)
            [\1 \0]
            [\0 \1]))
      gamma (->> s
                 (map first)
                 str/join
                 parse-binary)
      epsilon (->> s
                   (map second)
                   str/join
                   parse-binary)]
  (* gamma epsilon))
; => 3847100

;; Part 2
(defn most-common-bit [coll index]
  (let [{zeroes \0
         ones   \1} (->> coll
                         (map (fn [x] (nth x index)))
                         frequencies)]
    (if (<= zeroes ones) \1 \0)))

(defn least-common-bit [coll index]
  (let [{zeroes \0
         ones   \1} (->> coll
                         (map (fn [x] (nth x index)))
                         frequencies)]
    (if (<= zeroes ones) \0 \1)))

(let [[[oxygen] _] (->> [input 0]
                        (iterate (fn [[s index]]
                                   (let [bit (most-common-bit s index)]
                                     [(filter (fn [n]
                                                (= (nth n index) bit))
                                              s)
                                      (inc index)])))
                        (drop-while (comp not #{1} count first))
                        first)
      [[co2] _] (->> [input 0]
                     (iterate (fn [[s index]]
                                (let [bit (least-common-bit s index)]
                                  [(filter (fn [n]
                                             (= (nth n index) bit))
                                           s)
                                   (inc index)])))
                     (drop-while (comp not #{1} count first))
                     first)]
  (* (parse-binary oxygen)
     (parse-binary co2)))
; => 4105235
