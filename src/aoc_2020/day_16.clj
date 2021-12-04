(ns aoc-2020.day-16
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            [clojure.math.numeric-tower :as math]
            [medley.core :as medley]
            [aoc.util :refer :all]))

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true))

(defn parse-input [input-str]
  (let [[rules
         [_ my-ticket]
         [_ & nearby-tickets]] (->> input-str
                                    str/split-lines
                                    (partition-by str/blank?)
                                    (remove (comp-> first str/blank?)))
        rules (into {}
                    (map (fn [rule]
                           (let [[_ field n1 n2 n3 n4]
                                 (re-find #"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)" rule)]
                             [field
                              [[(parse-number n1)
                                (parse-number n2)]
                               [(parse-number n3)
                                (parse-number n4)]]])))
                    rules)
        parse-ticket (fn [s]
                       (mapv parse-number (str/split s #",")))
        my-ticket (parse-ticket my-ticket)
        nearby-tickets (mapv parse-ticket nearby-tickets)]
    {:rules rules
     :my-ticket my-ticket
     :nearby-tickets nearby-tickets}))

(def input
  (parse-input (slurp (io/resource "2020/day16.txt"))))

;; Part 1
(defn matches-rule? [[rule-name [[n1 n2] [n3 n4]]] n]
  (or (<= n1 n n2)
      (<= n3 n n4)))

(defn sum-invalid-numbers [rules ticket]
  (transduce (map (fn [n]
                    (if (every? (fn [rule]
                                  (not (matches-rule? rule n)))
                                rules)
                      n
                      0)))
             +
             ticket))

(->> (:nearby-tickets input)
     (map (partial sum-invalid-numbers (:rules input)))
     (reduce +))
;=> 24021

;; Part 2
(let [tickets (->> (:nearby-tickets input)
                   (filter (fn [ticket]
                             (every? (fn [n]
                                       (some (fn [rule]
                                               (matches-rule? rule n))
                                             (:rules input)))
                                     ticket)))
                   (into [(:my-ticket input)]))
      index-field (->> (for [index (range (count (:my-ticket input)))]
                         (for [rule (:rules input)
                               :when (every? (partial matches-rule? rule)
                                             (map (fn [ticket] (nth ticket index))
                                                  tickets))]
                           [index (first rule)]))
                       (into [] cat))
      sol-index->field (loop [sol-index->field {}
                              index-field index-field]
                         (if (empty? index-field)
                           sol-index->field
                           (let [index->field (group-by first second conj #{} index-field)
                                 sol-index->field (->> index->field
                                                       (filter (comp-> second count #{1}))
                                                       (map (fn [[k vs]] [k (first vs)]))
                                                       (into sol-index->field))
                                 sol-indexes (set (keys sol-index->field))
                                 index-field (->> index-field
                                                  (remove (fn [[index field]] (sol-indexes index))))

                                 field->index (group-by second first conj #{} index-field)
                                 sol-index->field (->> field->index
                                                       (filter (comp-> second count #{1}))
                                                       (map (fn [[k vs]] [(first vs) k]))
                                                       (into sol-index->field))
                                 sol-fields (set (vals sol-index->field))
                                 index-field (->> index-field
                                                  (remove (fn [[index field]] (sol-fields field))))]
                             (recur sol-index->field index-field))))]
  (->> sol-index->field
       (filter (fn [[k v]]
                 (str/starts-with? v "departure")))
       (map (fn [[index field]]
              (nth (:my-ticket input) index)))
       (apply *)))
;=> 1289178686687
