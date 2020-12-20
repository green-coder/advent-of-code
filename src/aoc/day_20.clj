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

;; Demo input
(def input (parse-input "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
"))

; Real input
(def input
  (parse-input (slurp (io/resource "day20.txt"))))

;; Part 1
(defn flip-cols [tile]
  (mapv (comp-> reverse #(apply str %)) tile))

(defn flip-rows [tile]
  (vec (reverse tile)))

(defn rot-cw [tile]
  (let [rows (count tile)
        cols (count (first tile))]
    (vec (for [c (range cols)]
           (apply str (for [r (range rows)]
                        (get-in tile [(- rows 1 r) c])))))))

(defn border->number [chars]
  (parse-binary (apply str (map {\# \1 \. \0} chars))))

(defn get-borders [tile]
  ; t r b l
  [(border->number (seq (first tile)))
   (border->number (map last tile))
   (border->number (seq (last tile)))
   (border->number (map first tile))])

(defn all-variants [tile]
  (let [elit (flip-cols tile)]
    [tile
     (rot-cw tile)
     (rot-cw (rot-cw tile))
     (rot-cw (rot-cw (rot-cw tile)))
     elit
     (rot-cw elit)
     (rot-cw (rot-cw elit))
     (rot-cw (rot-cw (rot-cw elit)))]))

(def graph
  (->> input
       (into {}
             (map (fn [[tile-id tile-content]]
                    [tile-id
                     (mapv get-borders (all-variants tile-content))])))))

;; find top
(defn travel [dir-index tile-id borders seen]
  (let [direction #(nth % dir-index)
        inv-direction #(nth % (mod (+ dir-index 2) 4))]
    (loop [seen seen
           tile-id tile-id
           borders borders]
      (let [border (direction borders)
            [next-tile-id next-borders]
            (->> graph
                 (keep (fn [[this-tile-id possible-borders]]
                         (when-let [borders (->> possible-borders
                                                 (filter (comp-> inv-direction #{border}))
                                                 first)]
                           [this-tile-id borders])))
                 (remove (comp-> first seen))
                 first)]
        (if (nil? next-tile-id)
          [tile-id borders seen]
          (recur (conj seen next-tile-id)
                 next-tile-id
                 next-borders))))))

(defn corner [dir1 dir2]
  (let [tile-id (ffirst graph)]
    (->> [tile-id (first (graph tile-id)) #{tile-id}]
         (apply (partial travel dir1))
         (apply (partial travel dir2)))))

#_ (* (first (corner 0 1))
      (first (corner 0 3))
      (first (corner 2 1))
      (first (corner 2 3)))
;=> 18262194216271

;; Part 2

;(corner 0 1)
(def top-left-corner
  (vec (take 2 (corner 0 3))))
;(corner 2 1)
;(corner 2 3)

(defn neighbor [[tile-id borders] dir-index]
  (let [direction #(nth % dir-index)
        inv-direction #(nth % (mod (+ dir-index 2) 4))
        border (direction borders)]
    (->> graph
         (keep (fn [[this-tile-id possible-borders]]
                 (when (not= this-tile-id tile-id)
                   (when-let [borders (->> possible-borders
                                           (filter (comp-> inv-direction #{border}))
                                           first)]
                     [this-tile-id borders]))))
         first)))

(defn tile-id-borders->tile-content [[tile-id borders]]
  ;(prn tile-id)
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
