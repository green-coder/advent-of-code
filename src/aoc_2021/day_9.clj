(ns aoc-2021.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.util :as util :refer [comp->]]))

(def input
  (->> (io/resource "2021/day9.txt")
       slurp
       str/split-lines
       (mapv (fn [line]
               (mapv (comp-> str parse-long) line)))))

;; Part 1
(defn adjacents [[y x]]
  [[y (dec x)]
   [y (inc x)]
   [(dec y) x]
   [(inc y) x]])

(->> (for [[y row] (util/seq-indexed input)
           [x n] (util/seq-indexed row)
           :let [min-adj (->> (adjacents [y x])
                              (keep (fn [position]
                                      (get-in input position)))
                              (reduce min))]
           :when (< n min-adj)]
       (inc n))
     (reduce +))
;=> 631


;; Part 2
(->> (for [[y row] (util/seq-indexed input)
           [x n] (util/seq-indexed row)
           :let [min-adj (->> (adjacents [y x])
                              (keep (fn [position]
                                      (get-in input position)))
                              (reduce min))]
           :when (< n min-adj)]
       (loop [basin #{}
              edges #{[y x]}]
         (if (seq edges)
           (let [new-edges (into #{}
                                 (comp (mapcat adjacents)
                                       (remove (fn [pos]
                                                 (= (get-in input pos 9) 9)))
                                       (remove basin))
                                 edges)]
             (recur (into basin new-edges)
                    new-edges))
           (count basin))))
     (sort-by identity >)
     (take 3)
     (apply *))
; => 821560
