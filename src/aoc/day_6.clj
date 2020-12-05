(ns aoc.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            [medley.core :as medley]
            [aoc.util :as util]))


(def input
  (->> (line-seq (io/reader (io/resource "day6.txt")))
       (mapv (fn [line] line))))

;; Part 1
(->> input)

;; Part 2.
(->> input)
