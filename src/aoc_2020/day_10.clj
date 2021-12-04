(ns aoc-2020.day-10
  (:refer-clojure :exclude [group-by])
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

;; Demo input 1
(def input
  (parse-input "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"))

;; Demo input 2
(def input
  (parse-input "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"))

;; Real input
(def input
  (parse-input (slurp (io/resource "2020/day10.txt"))))


;; Part 1
(let [distrib (->> (conj input 0 (+ (reduce max input) 3))
                   sort
                   vec
                   (partitionv 2 1)
                   (mapv (fn [[a b]] (- b a)))
                   frequencies)]
  (* (distrib 1) (distrib 3)))
; 1914

;; Part 2
(def solve
  (let [input (->> (conj input 0 (+ (reduce max input) 3))
                   sort
                   vec)]
    (memoize
      (fn [prev-val index]
        (if (< index (dec (count input)))
          (let [val (input index)
                next-val (input (inc index))]
            (if (<= (- next-val prev-val) 3)
              (+ (solve val (inc index))
                 (solve prev-val (inc index)))
              (solve val (inc index))))
          1)))))

(solve 0 1)
; 9256148959232
