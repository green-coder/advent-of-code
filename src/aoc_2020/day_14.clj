(ns aoc-2020.day-14
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (->> input-str
       str/split-lines
       (mapv (fn [line]
               (if-let [[_ mask] (re-find #"^mask = ([X01]*)$" line)]
                 [:mask mask]
                 (let [[_ addr val] (re-find #"^mem\[(\d*)\] = (\d*)$" line)]
                   [:mem (parse-number addr) (parse-number val)]))))))


(def input
  (parse-input (slurp (io/resource "2020/day14.txt"))))

;; Part 1
(->> (reduce (fn [[x-mask v-mask memory] [op arg1 arg2]]
               (case op
                 :mask [(parse-binary (str/escape arg1 {\1 \0, \X \1}))
                        (parse-binary (str/escape arg1 {\X \0}))
                        memory]
                 :mem [x-mask v-mask
                       (assoc memory
                         arg1 (bit-or (bit-and arg2 x-mask) v-mask))]))
             [0 0 {}]
             input)
     last
     vals
     (reduce +))
;=> 13496669152158


;; Part 2 - Naive approach
(defn get-addresses [mask]
  (reduce (fn [acc m-bit] acc
            (case m-bit
              \0 (map (fn [n]
                        (bit-shift-left n 1))
                      acc)
              \1 (map (fn [n]
                        (inc (bit-shift-left n 1)))
                      acc)
              \X (mapcat (fn [n]
                           (let [n (bit-shift-left n 1)]
                             [n (inc n)]))
                         acc)))
          [0]
          mask))

(->> (reduce (fn [[mask memory] [op arg1 arg2]]
               (case op
                 :mask [arg1 memory]
                 :mem [mask (let [n (Long/toBinaryString arg1)
                                  addr-mask (apply str (reverse (map (fn [n m]
                                                                       (if (= \0 m) n m))
                                                                     (concat (reverse n) (repeat \0))
                                                                     (reverse mask))))]
                              (reduce (fn [acc addr]
                                        (assoc acc addr arg2))
                                      memory
                                      (get-addresses addr-mask)))]))
             ["" {}]
             input)
     last
     vals
     (reduce +))
;=> 3278997609887
