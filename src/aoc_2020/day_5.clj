(ns aoc-2020.day-5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            [clojure.edn :as edn]
            [medley.core :as medley]))


(def input
  (->> (line-seq (io/reader (io/resource "2020/day5.txt")))
       (map (fn [line] line))))


(defn seat-id [seating]
  (let [row (->> seating
                 (take 7)
                 (map {\F \0 \B \1})
                 (apply str "2r")
                 (edn/read-string))
        col (->> seating
                 (drop 7)
                 (take 3)
                 (map {\L \0 \R \1})
                 (apply str "2r")
                 (edn/read-string))]
    (+ (* row 8) col)))

#_ (seat-id "FBFBBFFRLR")

;; Part 1
(def max-seat-id
  (->> input
       (transduce (map seat-id) max 0)))

;; Part 2.
(def min-seat-id
  (->> input
       (transduce (map seat-id) min ##Inf)))

(->> input
     (into #{} (map seat-id))
     (set/difference (into #{} (range min-seat-id (inc max-seat-id)))))
