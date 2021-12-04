(ns aoc-2020.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            [medley.core :as medley]
            [aoc.util :as util]))


(def input
  (-> (slurp (io/reader (io/resource "2020/day6.txt")))
      (str/split #"\R\R")
      (->> (mapv str/split-lines))))

;; Part 1
(->> input
     (map (fn [group]
            (count (into #{} cat group))))
     (reduce +))

;; Part 2.
(->> input
     (map (fn [group]
            (->> (frequencies (apply str group))
                 (transduce (keep (fn [[k v]]
                                    (when (= v (count group))
                                      1)))
                            +))))
     (reduce +))

