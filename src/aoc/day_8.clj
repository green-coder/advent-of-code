(ns aoc.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            [medley.core :as medley]
            [aoc.util :as util]))

(def input
  (-> (line-seq (io/reader (io/resource "day8.txt")))
      (->> (mapv (fn [line]
                   (let [[_ op sign n] (re-find #"(nop|jmp|acc) (-|\+)(\d+)" line)]
                     [op (cond-> (util/parse-number n)
                                 (= sign "-") (* -1))]))))))

;; Part 1
(defn simulate [program]
  (loop [ip 0
         acc 0
         seen #{}]
    (if (or (seen ip) (>= ip (count program)))
      [acc ip]
      (let [[op val] (program ip)
            seen (conj seen ip)]
        (case op
          "nop" (recur (inc ip) acc seen)
          "acc" (recur (inc ip) (+ acc val) seen)
          "jmp" (recur (+ ip val) acc seen))))))

(first (simulate input))

;; Part 2.
(->> (range (count input))
     (keep (fn [index]
             (let [[op val] (input index)]
               (when-let [new-op ({"jmp" "nop", "nop" "jmp"} op)]
                 (assoc input index [new-op val])))))
     (map simulate)
     (filter (fn [[acc ip]]
               (>= ip (count input))))
     ffirst)
