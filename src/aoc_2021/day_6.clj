(ns aoc-2021.day-6
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "2021/day6.txt")
       slurp
       (re-seq #"\d+")
       (map parse-long)))

(defn simulate [state]
  (let [s                   (update-keys state dec)
        fish-count-to-reset (s -1)]
    (if (nil? fish-count-to-reset)
      s
      (-> s
          (dissoc -1)
          (update 6 (fnil + 0) fish-count-to-reset)
          (assoc 8 fish-count-to-reset)))))

(defn population-count [n-days]
  (let [initial-state (frequencies input)]
    (-> (iterate simulate initial-state)
        (nth n-days)
        vals
        (->> (reduce +)))))

;; Part 1
(population-count 80)
; => 355386

;; Part 2
(population-count 256)
; => 1613415325809
