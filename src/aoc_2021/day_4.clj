(ns aoc-2021.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sections
  (-> (io/resource "2021/day4.txt")
      slurp
      (str/split #"\n\n")))

(def numbers
  (->> (sections 0)
       (re-seq #"\d+")
       (map parse-long)))

(def grids
  (for [section (subvec sections 1)]
    (let [elements (->> section
                        (re-seq #"\d+")
                        (map parse-long)
                        (partition 5))
          rows (map set elements)
          cols (for [index (range 5)]
                 (set (map #(nth % index) elements)))]
      (-> []
          (into rows)
          (into cols)))))

;; Part 1
(defn find-winner [numbers grids]
  (loop [[n & next-numbers] numbers
         grids grids]
    (let [grids (for [grid grids]
                  (for [s grid]
                    (disj s n)))
          solution (-> (for [grid grids
                             :when (some empty? grid)]
                         (-> (* n (transduce cat + grid))
                             (/ 2)))
                       first)]
      (if (some? solution)
        solution
        (recur next-numbers grids)))))

(find-winner numbers grids)
;;=> 49686

;; Part 2
(defn find-loser [numbers grids]
  (loop [[n & next-numbers] numbers
         grids grids]
    (let [grids (for [grid grids]
                  (for [s grid]
                    (disj s n)))
          solution (-> (for [grid grids
                             :when (some empty? grid)]
                         (-> (* n (transduce cat + grid))
                             (/ 2)))
                       first)
          grids (->> grids
                     (remove (fn [grid]
                               (some empty? grid))))]
      (if (and (empty? grids)
               (some? solution))
        solution
        (recur next-numbers grids)))))

(find-loser numbers grids)
;;=> 26878
