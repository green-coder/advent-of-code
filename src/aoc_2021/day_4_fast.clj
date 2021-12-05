(ns aoc-2021.day-4-fast
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sections
  (-> (io/resource "2021/day4.txt")
      slurp
      (str/split #"\n\n")))

(def numbers
  (->> (sections 0)
       (re-seq #"\d+")
       (mapv parse-long)))

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

(def number->index
  (into {} (map-indexed (fn [i n] [n i])) numbers))

(defn winning-index [grid]
  (->> grid
       (mapv (fn [line]
               (->> line
                    (map number->index)
                    (apply max))))
       (apply min)))

(def winning-index->grid
  (into {} (map (juxt winning-index identity)) grids))

(defn solution [winning-index]
  (let [number (numbers winning-index)
        grid   (winning-index->grid winning-index)]
    (-> (transduce (mapcat (fn [line]
                             (->> line
                                  (filter (fn [x] (> (number->index x) winning-index))))))
                   +
                   grid)
        (/ 2)
        (* number))))

;; Part 1
(solution (apply min (keys winning-index->grid)))
;;=> 49686

;; Part 2
(solution (apply max (keys winning-index->grid)))
;;=> 26878
