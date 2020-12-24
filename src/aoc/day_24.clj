(ns aoc.day-24
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (->> (str/split-lines input-str)
       (mapv (fn [line]
               (mapv keyword (re-seq #"e|se|sw|w|nw|ne" line))))))

(def input
  (parse-input (slurp (io/resource "day24.txt"))))

;; Part 1
(def dir->coord
  {:nw [ 0  1]
   :ne [ 1  1]
   :e  [ 1  0]
   :se [ 0 -1]
   :w  [-1  0]
   :sw [-1 -1]})

(defn get-tile-pos [line]
  (let [coords (map dir->coord line)]
    [(transduce (map first) + coords)
     (transduce (map second) + coords)]))

(defn flip-tile [all-blacks tile-pos]
  (if (all-blacks tile-pos)
    (disj all-blacks tile-pos)
    (conj all-blacks tile-pos)))

(->> input
     (map get-tile-pos)
     (reduce flip-tile #{})
     count)
;=> 307

;; Part 2
(defn neighbors-pos [[x y]]
  [[     x  (inc y)]
   [(inc x) (inc y)]
   [(inc x)      y]
   [     x  (dec y)]
   [(dec x)      y]
   [(dec x) (dec y)]])

(defn daily-flip [all-blacks]
  (let [staying-blacks (keep (fn [black-pos]
                               (when (#{1 2} (->> (neighbors-pos black-pos)
                                                 (filter all-blacks)
                                                 count))
                                 black-pos))
                             all-blacks)
        all-white-neighbors (into #{}
                                  (comp (mapcat neighbors-pos)
                                        (remove all-blacks))
                                  all-blacks)
        appearing-blacks (keep (fn [white-pos]
                                 (when (= (->> (neighbors-pos white-pos)
                                               (filter all-blacks)
                                               count)
                                          2)
                                   white-pos))
                               all-white-neighbors)]
    (set (concat staying-blacks appearing-blacks))))

(->> input
     (map get-tile-pos)
     (reduce flip-tile #{})
     (iterate daily-flip)
     (drop 100)
     first
     count)
;=> 3787
