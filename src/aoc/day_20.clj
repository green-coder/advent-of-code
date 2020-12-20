(ns aoc.day-20
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (->> (str/split input-str #"\R\R")
       (mapv (fn [tile]
               (let [[head & rows] (str/split-lines tile)
                     id (parse-number (re-find #"\d+" head))
                     rows (vec rows)]
                 [id rows])))
       (into {})))

(def input
  (parse-input (slurp (io/resource "day20.txt"))))

;; Part 1
(defn flip-rows [tile-content]
  (vec (reverse tile-content)))

(defn rot-cw [tile-content]
  (let [rows (count tile-content)
        cols (count (first tile-content))]
    (forv [c (range cols)]
      (apply str (for [r (range rows)]
                   (get-in tile-content [(- rows 1 r) c]))))))

(defn border->number [chars]
  (parse-binary (apply str (map {\# \1 \. \0} chars))))

(defn get-borders [tile-content]
  ; [top right bottom left]
  (mapv border->number [(seq (first tile-content))
                        (map last tile-content)
                        (seq (last tile-content))
                        (map first tile-content)]))

(defn all-variants [tile]
  (vec (concat (take 4 (iterate rot-cw tile))
               (take 4 (iterate rot-cw (flip-rows tile))))))

(def graph
  (->> input
       (map (fn [[tile-id tile-content]]
              [tile-id (mapv get-borders (all-variants tile-content))]))
       (into {})))

(defn neighbor [[tile-id borders] dir-index]
  (let [direction #(nth % dir-index)
        inv-direction #(nth % (mod (+ dir-index 2) 4))
        border (direction borders)]
    (->> graph
         (some (fn [[this-tile-id possible-borders]]
                 (when (not= this-tile-id tile-id)
                   (when-let [borders (->> possible-borders
                                           (filter (comp-> inv-direction #{border}))
                                           first)]
                     [this-tile-id borders])))))))

(defn corner [dir1 dir2]
   (let [[tile-id [borders]] (first graph)]
     (->> [tile-id borders]
          (iterate #(neighbor % dir1))
          (take-while some?)
          last
          (iterate #(neighbor % dir2))
          (take-while some?)
          last)))

(->> [(corner 0 1)
      (corner 0 3)
      (corner 2 1)
      (corner 2 3)]
     (map first)
     (apply *))
;=> 18262194216271


;; Part 2

(def top-left-corner
  (vec (take 2 (corner 0 3))))

(defn tile-id-borders->tile-content [[tile-id borders]]
  (let [variants (all-variants (input tile-id))
        borders->variant (into {}
                               (map vector
                                    (mapv get-borders variants)
                                    variants))]
    (borders->variant borders)))

(defn trim-borders [tile-content]
  (mapv (fn [line]
          (apply str (next (butlast line))))
        (subvec tile-content 1 (dec (count tile-content)))))

(def picture
  (let [tiles (forv [left (->> top-left-corner
                               (iterate #(neighbor % 2))
                               (take-while some?))]
                (forv [each (->> left
                                 (iterate #(neighbor % 1))
                                 (take-while some?))]
                  (->> (tile-id-borders->tile-content each)
                       trim-borders)))
        row-count (count tiles)
        col-count (count (first tiles))
        tile-height (count (first (first tiles)))]
    [row-count col-count tile-height]
    (forv [row (range row-count)
           tile-row (range tile-height)]
      (apply str (for [col (range col-count)]
                   (get-in tiles [row col tile-row]))))))


(def monster
  ["..................#."
   "#....##....##....###"
   ".#..#..#..#..#..#..."])

(def all-monsters
  (all-variants monster))

(defn is-monster-at? [monster pr pc]
  (let [correlations (for [mr (range (count monster))
                           mc (range (count (first monster)))]
                       (if (= \#
                              (get-in monster [mr mc] \.)
                              (get-in picture [(+ pr mr) (+ pc mc)] \.))
                         1
                         0))]
    (= (reduce + correlations) 15)))

(def found-monsters
  (for [pr (range (count picture))
        pc (range (count (first picture)))
        monster all-monsters
        :when (is-monster-at? monster pr pc)]
    [monster pr pc]))

(defn is-on-monster? [pr pc monster mr mc]
  (= \# (get-in monster [(- pr mr) (- pc mc)] \.)))

(-> (for [pr (range (count picture))
          pc (range (count (first picture)))
          :when (= (get-in picture [pr pc]) \#)
          :when (every? (fn [[monster mr mc]]
                          (not (is-on-monster? pr pc monster mr mc)))
                        found-monsters)]
      [pr pc])
    count)
;=> 2023
