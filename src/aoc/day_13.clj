(ns aoc.day-13
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (let [[t0 ids] (str/split-lines input-str)]
    [(parse-number t0)
     (->> (keep (fn [x]
                  (when-not (#{"x"} x)
                    (parse-number x)))
                (str/split ids #","))
          sort
          vec)]))

(defn parse-input2 [input-str]
  (let [[t0 ids] (str/split-lines input-str)]
    (->> (mapv (fn [x]
                 (if (#{"x"} x)
                   :x
                   (parse-number x)))
               (str/split ids #","))
         (map-indexed (fn [i x] [x i]))
         (filter (comp-> first number?))
         vec)))

(def input (slurp (io/resource "day13.txt")))

;; Part 1
(let [[t0 ids] (parse-input input)]
  (->> (into []
             (map (fn [id]
                    [(mod (- id t0) id) id]))
             ids)
       (sort-by first)
       first
       (apply *)))
;=> 3035

;; Part 2.
(defn solve [constraints]
  (->> (reduce (fn [[d1 r1] [d2 r2]]
                 (let [n (->> (iterate (partial + d1) r1)
                              (drop-while (fn [x] (not= 0 (mod (+ x r2) d2))))
                              first)
                       d (* d1 d2)]
                   [d (mod n d)]))
               constraints)
       second))

(solve (parse-input2 input))
;=> 725169163285238
