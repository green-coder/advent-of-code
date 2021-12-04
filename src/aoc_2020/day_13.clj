(ns aoc-2020.day-13
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [criterium.core :as crit]
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

(def input (slurp (io/resource "2020/day13.txt")))

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
(def parsed-input (parse-input2 input))

(defn solve [constraints]
  (->> (reduce (fn [[d1 r1] [d2 r2]]
                 (let [n (->> (iterate (partial + d1) r1)
                              (drop-while (fn [x] (not= 0 (mod (+ x r2) d2))))
                              first)
                       d (* d1 d2)]
                   [d (mod n d)]))
               constraints)
       second))

(solve parsed-input)
;=> 725169163285238

;; Part 2 optimization 1
(defn solve-fast-1 [constraints]
  (->> (reduce (fn [[d1 r1] [d2 r2]]
                 (let [n (- (->> (iterate (partial + d1) (+ r1 r2))
                                 (drop-while (fn [x] (not= 0 (mod x d2))))
                                 first)
                            r2)
                       d (* d1 d2)
                       r (mod n d)]
                   [d r]))
               constraints)
       second))

;; Part 2 optimization 2 (the best)
(defn solve-fast-2 [constraints]
  (let [divs (map first constraints)
        rems (map (comp-> second -) constraints)
        product (reduce * divs)
        subproducts (map #(/ product %) divs)
        inv-mods (map (fn [sp d]
                        (int (.modInverse (biginteger sp) (biginteger d))))
                      subproducts
                      divs)]
    (mod (apply + (map * inv-mods subproducts rems))
         product)))

(comment
  ;; Benchmarking
  (crit/quick-bench (solve parsed-input))          ;; 111.743998 µs
  (crit/quick-bench (solve-fast-1 parsed-input))   ;; 101.005388 µs
  (crit/quick-bench (solve-fast-2 parsed-input)))  ;; 19.709029 µs
