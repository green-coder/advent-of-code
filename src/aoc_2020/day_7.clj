(ns aoc-2020.day-7
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            [medley.core :as medley]
            [aoc.util :refer :all]))

(defn parse-input [input]
  (->> input
       (map (fn [line]
              (let [[_ out-color right-expr] (re-matches #"(.*) bags contain (.*)." line)
                    contained-exprs (if (= right-expr "no other bags")
                                      []
                                      (str/split right-expr #", "))
                    in-color (into {}
                                   (map (fn [contained-expr]
                                         (let [[_ n in-color] (re-matches #"(\d+) (.*) bags?" contained-expr)]
                                           [in-color (parse-number n)])))
                                   contained-exprs)]
                [out-color in-color])))
       (into {})))

;; Demo input 1
(def input
  (->> "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."
       str/split-lines
       parse-input))

;; Demo input 2
(def input
  (->> "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."
       str/split-lines
       parse-input))

;; Real input
(def input
  (->> (line-seq (io/reader (io/resource "2020/day7.txt")))
       parse-input))

(def reverse-input
  (->> input
       (mapcat (fn [[out ins]]
                 (map (fn [in] [in out]) (keys ins))))
       (group-by first second conj #{})))

;; Part 1
(loop [visited #{}
       frontier ["shiny gold"]]
  (if (empty? frontier)
    (dec (count visited))
    (let [color (peek frontier)
          visited (conj visited color)
          frontier (into (pop frontier)
                         (remove visited)
                         (reverse-input color))]
      (recur visited frontier))))
; 131

;; Part 2.
(def solve
  (memoize (fn [outter-color]
             (->> (input outter-color)
                  (transduce (map (fn [[inner-coloc factor]]
                                    (* (solve inner-coloc) factor)))
                             +
                             1)))))

(dec (solve "shiny gold"))
; 11261
