(ns aoc-2020.day-12
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
       (mapv (fn [line]
               (let [[c & n] line]
                 [c (parse-number (apply str n))])))))

;; Demo input
(def input
  (parse-input "F10\nN3\nF7\nR90\nF11"))

;; Real input
(def input
  (parse-input (slurp (io/resource "2020/day12.txt"))))

(def dir-v
  {:east [0 1]
   :west [0 -1]
   :north [1 0]
   :south [-1 0]})

(def left-of
  {:east :north
   :north :west
   :west :south
   :south :east})

(def right-of
  {:east :south
   :south :west
   :west :north
   :north :east})

;; Part 1
(let [[n e] (reduce (fn [[north east dir] [c n]]
                      (case c
                        \N [(+ north (* (-> dir-v :north (nth 0)) n))
                            (+ east (* (-> dir-v :north (nth 1)) n))
                            dir]
                        \S [(+ north (* (-> dir-v :south (nth 0)) n))
                            (+ east (* (-> dir-v :south (nth 1)) n))
                            dir]
                        \E [(+ north (* (-> dir-v :east (nth 0)) n))
                            (+ east (* (-> dir-v :east (nth 1)) n))
                            dir]
                        \W [(+ north (* (-> dir-v :west (nth 0)) n))
                            (+ east (* (-> dir-v :west (nth 1)) n))
                            dir]
                        \L [north east (-> (iterate left-of dir)
                                           (nth (quot n 90)))]
                        \R [north east (-> (iterate right-of dir)
                                           (nth (quot n 90)))]
                        \F [(+ north (* (-> dir-v dir (nth 0)) n))
                            (+ east (* (-> dir-v dir (nth 1)) n))
                            dir]))
                    [0 0 :east]
                    input)]
  (manhattan-dist n e))
; => 2879


;; Part 2.
(defn ccw-rot-90 [[w e]]
  [e (neg w)])

(defn cw-rot-90 [[w e]]
  [(neg e) w])

(let [[n e] (reduce (fn [[north east wn we] [c n]]
                      (case c
                        \N [north east (+ wn n) we]
                        \S [north east (- wn n) we]
                        \E [north east wn (+ we n)]
                        \W [north east wn (- we n)]
                        \L (let [[wn we] (-> (iterate ccw-rot-90 [wn we])
                                             (nth (quot n 90)))]
                             [north east wn we])
                        \R (let [[wn we] (-> (iterate cw-rot-90 [wn we])
                                             (nth (quot n 90)))]
                             [north east wn we])
                        \F [(+ north (* n wn))
                            (+ east (* n we))
                            wn we]))
                    [0 0 1 10]
                    input)]
  (manhattan-dist n e))
; => 178986
