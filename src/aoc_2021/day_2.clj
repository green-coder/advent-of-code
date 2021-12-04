(ns aoc-2021.day-2
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "2021/day2.txt")
       io/reader
       line-seq
       (mapv (fn [line]
               (let [[_ dir n] (re-find #"(up|down|forward) (-?\d+)" line)
                     dir (keyword dir)
                     n (parse-long n)]
                 [dir n])))))

;; Part 1
(as-> (->> input
           (reduce (fn [acc [dir n]]
                     (case dir
                       :forward (update acc :horizontal + n)
                       :up (update acc :depth - n)
                       :down (update acc :depth + n)))
                   {:horizontal 0
                    :depth 0}))
      {:keys [horizontal depth]}
      (* horizontal depth))
; => 1499229


;; Part 2
(as-> (->> input
           (reduce (fn [acc [dir n]]
                     (case dir
                       :forward (-> acc
                                    (update :horizontal + n)
                                    (update :depth + (* (:aim acc) n)))
                       :up (update acc :aim - n)
                       :down (update acc :aim + n)))
                   {:aim 0
                    :horizontal 0
                    :depth 0}))
      {:keys [horizontal depth]}
      (* horizontal depth))
; => 1340836560
