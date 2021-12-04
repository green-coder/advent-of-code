(ns aoc-2020.day-17
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (->> input-str
       str/split-lines
       (mapv (fn [line] (vec line)))))

;; Demo input
(def input
  (parse-input ".#.
..#
###"))

;; Real input
(def input
  (parse-input (slurp (io/resource "2020/day17.txt"))))

;; Part 1
(defn create-space-3 [slice]
  (let [nb-rows (count slice)
        nb-cols (count (first slice))]
    (->> (for [x (range nb-rows)
               y (range nb-cols)
               :when (= (-> slice (nth x) (nth y)) \#)]
           [x y 0])
         set)))

(defn nb-neighbors-3 [space x y z]
  (->> (for [nx (range -1 2)
             ny (range -1 2)
             nz (range -1 2)
             :when (not= [nx ny nz] [0 0 0])]
         [(+ x nx) (+ y ny) (+ z nz)])
       (filter space)
       count))

(defn life-3 [x-min x-sup y-min y-sup z-min z-sup space]
  (->> (for [x (range x-min x-sup)
             y (range y-min y-sup)
             z (range z-min z-sup)
             :let [active? (space [x y z])
                   n (nb-neighbors-3 space x y z)
                   become-active? (if active?
                                    (#{2 3} n)
                                    (= 3 n))]
             :when become-active?]
         [x y z])
       set))

(let [nb-rows (count input)
      nb-cols (count (first input))]
  (loop [space (create-space-3 input)
         cycle 0]
    (if (= 6 cycle)
      (count space)
      (let [cycle (inc cycle)
            space (life-3 (- 0 cycle)
                          (+ nb-rows cycle)
                          (- 0 cycle)
                          (+ nb-cols cycle)
                          (- 0 cycle)
                          (+ 1 cycle)
                          space)]
          (recur space cycle)))))
;=> 448


;; Part 2
(defn create-space-4 [slice]
  (let [nb-rows (count slice)
        nb-cols (count (first slice))]
    (->> (for [x (range nb-rows)
               y (range nb-cols)
               :when (= (-> slice (nth x) (nth y)) \#)]
           [x y 0 0])
         set)))

(defn nb-neighbors-4 [space x y z w]
  (->> (for [nx (range -1 2)
             ny (range -1 2)
             nz (range -1 2)
             nw (range -1 2)
             :when (not= [nx ny nz nw] [0 0 0 0])]
         [(+ x nx) (+ y ny) (+ z nz) (+ w nw)])
       (filter space)
       count))

(defn life-4 [x-min x-sup y-min y-sup z-min z-sup w-min w-sup space]
  (->> (for [x (range x-min x-sup)
             y (range y-min y-sup)
             z (range z-min z-sup)
             w (range w-min w-sup)
             :let [active? (space [x y z w])
                   n (nb-neighbors-4 space x y z w)
                   become-active? (if active?
                                    (#{2 3} n)
                                    (= 3 n))]
             :when become-active?]
         [x y z w])
       set))

(let [nb-rows (count input)
      nb-cols (count (first input))]
  (loop [space (create-space-4 input)
         cycle 0]
    (if (= 6 cycle)
      (count space)
      (let [cycle (inc cycle)
            space (life-4 (- 0 cycle)
                          (+ nb-rows cycle)
                          (- 0 cycle)
                          (+ nb-cols cycle)
                          (- 0 cycle)
                          (+ 1 cycle)
                          (- 0 cycle)
                          (+ 1 cycle)
                          space)]
          (recur space cycle)))))
;=> 2400
