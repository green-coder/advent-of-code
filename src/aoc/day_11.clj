(ns aoc.day-11
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            [medley.core :as medley]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (->> input-str
       str/split-lines
       (mapv vec)))

;; Demo input
(def input
  (parse-input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))

;; Real input
(def input
  (parse-input (slurp (io/resource "day11.txt"))))

;; Part 1
(def surroundings
  [[-1 -1] [-1 0] [-1 1]
   [0 -1] [0 1]
   [1 -1] [1 0] [1 1]])

(defn life [grid]
  (into []
        (map-indexed
          (fn [i-row row]
            (into []
                  (map-indexed
                    (fn [i-col elm]
                      (if (= \. elm)
                        elm
                        (let [nb-occupied-seats (transduce (map (fn [v]
                                                                  (if (= \# (-> grid
                                                                                 (nth (+ i-row (v 0)) [])
                                                                                 (nth (+ i-col (v 1)) \.)))
                                                                    1
                                                                    0)))
                                                           +
                                                           surroundings)]
                          (cond (zero? nb-occupied-seats) \#
                                (>= nb-occupied-seats 4) \L
                                :else elm)))))
                  row)))
        grid))

(time
  (let [grid (->> (iterate life input)
                  (partition 2 1)
                  (drop-while (fn [[left right]]
                                (not= left right)))
                  ffirst)]
    (->> grid
         flatten
         (filter #{\#})
         count)))

;; Part 2.

;; Divides the runtime in half.
(defmacro get-in
  ([coll [row col]]
   `(-> ~coll (nth ~row []) (nth ~col)))
  ([coll [row col] default]
   `(-> ~coll (nth ~row []) (nth ~col ~default))))

(defn seen-vertical [grid up? col-offset]
  (let [height (count grid)
        width (count (grid 0))
        acc (atom grid)]
    (forv [row ((if up? range reverse-range) height)
           :let [prev-row ((if up? dec inc) row)]]
      (forv [col (range width)]
        (let [prev-col (+ col col-offset)
              g (get-in grid [prev-row prev-col] \.)
              a (get-in @acc [prev-row prev-col] \.)
              c (if (= g \.) a g)]
          (swap! acc assoc-in [row col] c))))
    @acc))
#_ (seen-vertical (life input) true 0)

(defn seen-horizontal [grid left? row-offset]
  (let [height (count grid)
        width (count (grid 0))
        acc (atom grid)]
    (forv [col ((if left? range reverse-range) width)
           :let [prev-col ((if left? dec inc) col)]]
      (forv [row (range height)]
        (let [prev-row (+ row row-offset)
              g (get-in grid [prev-row prev-col] \.)
              a (get-in @acc [prev-row prev-col] \.)
              c (if (= g \.) a g)]
          (swap! acc assoc-in [row col] c))))
    @acc))
#_ (seen-horizontal (life input) true 0)

(defn life2 [grid]
  (let [up-left (seen-vertical grid true -1)
        up (seen-vertical grid true 0)
        up-right (seen-vertical grid true 1)
        down-left (seen-vertical grid false -1)
        down (seen-vertical grid false 0)
        down-right (seen-vertical grid false 1)
        left (seen-horizontal grid true 0)
        right (seen-horizontal grid false 0)
        tables [up up-right up-left
                down down-right down-left
                left  right]]
    (into []
          (map-indexed
            (fn [i-row row]
              (into []
                    (map-indexed
                      (fn [i-col elm]
                        (if (= \. elm)
                          elm
                          (let [nb-occupied-seats (transduce (map (fn [seen]
                                                                    (if (= \# (get-in seen [i-row i-col]))
                                                                      1
                                                                      0)))
                                                             +
                                                             tables)]
                            (cond (zero? nb-occupied-seats) \#
                                  (>= nb-occupied-seats 5) \L
                                  :else elm)))))
                    row)))
          grid)))

(time (let [grid (->> (iterate life2 input)
                      (partition 2 1)
                      (drop-while (fn [[left right]]
                                    (not= left right)))
                      (take 1)
                      ffirst)]
        (->> grid
             flatten
             (filter #{\#})
             count)))