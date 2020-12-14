(ns aoc.day-14
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


;; Demo input
(def input
  (parse-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"))

;; Demo input2
(def input
  (parse-input "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"))

;; Real input
(def input
  (parse-input (slurp (io/resource "day14.txt"))))

;; Part 1
(->> (reduce (fn [[n-mask x-mask v-mask memory] [op arg1 arg2]]
               ;(prn umask x-mask memory)
               (case op
                 :mask (let [n-mask (->> arg1
                                         (map {\0 1, \1 1, \X 0})
                                         (apply str)
                                         parse-binary)
                             x-mask (->> arg1
                                         (map {\0 0, \1 0, \X 1})
                                         (apply str)
                                         parse-binary)
                             v-mask (->> arg1
                                         (map {\0 0, \1 1, \X 0})
                                         (apply str)
                                         parse-binary)]
                         [n-mask x-mask v-mask memory])
                 :mem [n-mask x-mask v-mask
                       (assoc memory arg1
                                     (bit-or (bit-and arg2 x-mask) v-mask))]))
             [0 0 0 {}]
             input)
     last
     vals
     (reduce +))


;; Part 2 - Naive approach
(defn get-addresses [mask]
  (reduce (fn [acc m-bit] acc
            (case m-bit
              \0 (mapcat (fn [n]
                           [(bit-shift-left n 1)])
                         acc)
              \1 (mapcat (fn [n]
                           [(inc (bit-shift-left n 1))])
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
                                                                      (case m
                                                                        \0 n
                                                                        \1 \1
                                                                        \X))
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
