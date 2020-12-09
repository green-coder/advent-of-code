(ns aoc.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            [medley.core :as medley]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (->> input-str
       str/split-lines
       (mapv parse-number)))

(def input
  (parse-input (slurp (io/resource "day9.txt"))))

;; Part 1
(defn valid? [preamble n]
  (first-for [x (range (count preamble))
              y (range x (count preamble))
              :when (= (+ (preamble x)
                          (preamble y)) n)]
    [x y]))

(let [preamble-size 25]
  (first-for [i (range preamble-size (count input))
              :let [preamble (subvec input (- i preamble-size) i)
                    n (input i)]
              :when (not (valid? preamble n))]
    n))
;; 27911108

;; Part 2.
(let [target 27911108]
  (first-for [i (range (count input))
              :let [[sum -min -max] (reduce (fn [[sum -min -max] n]
                                              (let [x (+ sum n)
                                                    -min (min -min n)
                                                    -max (max -max n)]
                                                (cond-> [x -min -max]
                                                  (>= x target) reduced)))
                                            [(input i) (input i) (input i)]
                                            (subvec input (inc i)))]
              :when (= sum target)]
    (+ -min -max)))
;; 4023754
