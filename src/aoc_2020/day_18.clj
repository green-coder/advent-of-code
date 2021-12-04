(ns aoc-2020.day-18
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (->> input-str
       str/split-lines
       (mapv (fn [line]
               (edn/read-string (str "(" line ")"))))))

(def input
  (parse-input (slurp (io/resource "2020/day18.txt"))))

;; Part 1
(defn calc [line]
  (walk/postwalk (fn [x]
                   (if (sequential? x)
                     (reduce (fn [acc [op arg]]
                               (case op
                                 + (+ acc arg)
                                 * (* acc arg)))
                             (first x)
                             (partition 2 (next x)))
                     x))
                 line))

(->> input
     (map calc)
     (reduce +))

;; Part 2
(defn add-parents [line]
  (walk/postwalk (fn [x]
                   (if (sequential? x)
                     (->> x
                          (partition-by #{'*})
                          (map (fn [x]
                                 (if (= '(*) x) '* x))))
                     x))
                 line))

(->> input
     (map add-parents)
     (map calc)
     (reduce +))
