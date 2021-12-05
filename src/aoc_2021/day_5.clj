(ns aoc-2021.day-5
  (:require [clojure.java.io :as io]
            [aoc.util :as util]))

(def input
  (with-open [r (io/reader (io/resource "2021/day5.txt"))]
    (doall
      (->> (line-seq r)
           (map (fn [line]
                  (->> line
                       (re-seq #"\d+")
                       (map parse-long))))))))

;; Part 1
(defn from-to [a b]
  (if (< a b)
    (range a (inc b))
    (range b (inc a))))

(->> (reduce (fn [grid [x1 y1 x2 y2]]
               (cond
                 (= x1 x2) (reduce (fn [grid y]
                                     (update grid [x1 y] (fnil inc 0)))
                                   grid
                                   (from-to y1 y2))
                 (= y1 y2) (reduce (fn [grid x]
                                     (update grid [x y1] (fnil inc 0)))
                                   grid
                                   (from-to x1 x2))
                 :else grid))
             {}
             input)
     vals
     (filter (fn [x] (>= x 2)))
     count)
; => 5280

;; Part 2
(defn line-length [x1 y1 x2 y2]
  (max (Math/abs (- x1 x2))
       (Math/abs (- y1 y2))))

(->> (reduce (fn [grid [x1 y1 x2 y2]]
               (let [len (line-length x1 y1 x2 y2)
                     dx (/ (- x2 x1) len)
                     dy (/ (- y2 y1) len)
                     points (->> (iterate (fn [[x y]] [(+ x dx) (+ y dy)]) [x1 y1])
                                 (take (inc len)))]
                 (reduce (fn [grid coord]
                           (update grid coord (fnil inc 0)))
                         grid
                         points)))
             {}
             input)
     vals
     (filter (fn [x] (>= x 2)))
     count)
; => 16716
