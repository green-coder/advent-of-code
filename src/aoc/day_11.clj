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
                        (let [nb-occupied-seats (->> surroundings
                                                     (into []
                                                           (comp (map (fn [v]
                                                                         (-> grid
                                                                             (nth (+ i-row (v 0)) [])
                                                                             (nth (+ i-col (v 1)) \.))))
                                                                 (filter #{\#})))
                                                     count)]
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

(defn seen-up [grid x-offset]
  (let [height (count grid)
        width (count (grid 0))
        acc (atom grid)]
    (forv [i-row (range height)]
      (forv [i-col (range width)]
        (let [g (get-in grid [(dec i-row) (+ i-col x-offset)] \.)
              a (get-in @acc [(dec i-row) (+ i-col x-offset)] \.)
              c (if (= g \.) a g)]
          (swap! acc assoc-in [i-row i-col] c))))
    @acc))
;(seen-up (life input) 0)

(defn seen-down [grid x-offset]
  (let [height (count grid)
        width (count (grid 0))
        acc (atom grid)]
    (forv [i-row (range (dec height) -1 -1)]
      (forv [i-col (range width)]
        (let [g (get-in grid [(inc i-row) (+ i-col x-offset)] \.)
              a (get-in @acc [(inc i-row) (+ i-col x-offset)] \.)
              c (if (= g \.) a g)]
          (swap! acc assoc-in [i-row i-col] c))))
    @acc))

(defn seen-left [grid y-offset]
  (let [height (count grid)
        width (count (grid 0))
        acc (atom grid)]
    (forv [i-col (range width)]
      (forv [i-row (range height)]
        (let [g (get-in grid [(+ i-row y-offset) (dec i-col)] \.)
              a (get-in @acc [(+ i-row y-offset) (dec i-col)] \.)
              c (if (= g \.) a g)]
          (swap! acc assoc-in [i-row i-col] c))))
    @acc))

(defn seen-right [grid y-offset]
  (let [height (count grid)
        width (count (grid 0))
        acc (atom grid)]
    (forv [i-col (range (dec width) -1 -1)]
      (forv [i-row (range height)]
        (let [g (get-in grid [(+ i-row y-offset) (inc i-col)] \.)
              a (get-in @acc [(+ i-row y-offset) (inc i-col)] \.)
              c (if (= g \.) a g)]
          (swap! acc assoc-in [i-row i-col] c))))
    @acc))

;(seen-right (life input) 0)

(defn life2 [grid]
  (let [up-left (seen-up grid -1)
        up (seen-up grid 0)
        up-right (seen-up grid 1)
        down-left (seen-down grid -1)
        down (seen-down grid 0)
        down-right (seen-down grid 1)
        left (seen-left grid 0)
        right (seen-right grid 0)]
    (into []
          (map-indexed
            (fn [i-row row]
              (into []
                    (map-indexed
                      (fn [i-col elm]
                        (if (= \. elm)
                          elm
                          (let [neighbors (mapv (fn [seen]
                                                  (get-in seen [i-row i-col]))
                                                [up
                                                 up-right
                                                 up-left
                                                 down
                                                 down-right
                                                 down-left
                                                 left
                                                 right])
                                nb-occupied-seats (count (filter #{\#} neighbors))]
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
