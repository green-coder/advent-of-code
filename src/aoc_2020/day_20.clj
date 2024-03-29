(ns aoc-2020.day-20
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
  (parse-input (slurp (io/resource "2020/day20.txt"))))

;; Part 1
(defn border->number [chars]
  (parse-binary (apply str (map {\# \1 \. \0} chars))))

(defn get-borders [tile-content]
  ; [top right bottom left]
  (mapv border->number [(seq (first tile-content))
                        (map last tile-content)
                        (seq (last tile-content))
                        (map first tile-content)]))

(defn all-variants [tile]
  (vec (concat (take 4 (iterate rot2d-cw tile))
               (take 4 (iterate rot2d-cw (flip2d-rows tile))))))

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
        [row-count col-count tile-height] (get-dimensions tiles)]
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
  (let [correlations (for [[mr mc] (enum-coords monster)
                           :when (= \#
                                    (get-in monster [mr mc] \.)
                                    (get-in picture [(+ pr mr) (+ pc mc)] \.))]
                       [mr mc])]
    (= (count correlations) 15)))

;; Slow. 41 monsters found.
(def found-monsters
  (for [[pr pc] (enum-coords picture)
        monster all-monsters
        :when (is-monster-at? monster pr pc)]
    [monster pr pc]))

(defn is-on-monster? [pr pc monster mr mc]
  (= \# (get-in monster [(- pr mr) (- pc mc)] \.)))

(-> (for [[pr pc] (enum-coords picture)
          :when (= (get-in picture [pr pc]) \#)
          :when (every? (fn [[monster mr mc]]
                          (not (is-on-monster? pr pc monster mr mc)))
                        found-monsters)]
      [pr pc])
    count)
;=> 2023
