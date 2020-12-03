(ns aoc.day-4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            #_[clojure.core.logic :as logic]
            [medley.core :as medley]
            #_[ubergraph.core :as uber]
            #_[minimallist.core :as mini]
            #_[minimallist.helper :as h]
            #_[diffuse.core :as diff]
            #_[diffuse.helper :as dh]
            #_[lambdaisland.regal :as regal]))

(def input
  (->> (line-seq (io/reader (io/resource "day4.txt")))
       (take-while (complement str/blank?))
       (mapv (fn [line]
               line
               (let [[_ number letter] (re-find #"(\d+)(\w)" line)]
                 [(Long/parseLong number)
                  (first letter)])))))

;; Part 1
(->> input)

;; Part 2
(->> input)
