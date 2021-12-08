(ns aoc-2021.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [aoc.util :refer [comp->]]))

(def input
  (->> (io/resource "2021/day8.txt")
       slurp
       str/split-lines
       (mapv (fn [line]
               (vec (re-seq #"\w+" line))))))

;; Part 1
(->> input
     (mapcat (fn [line]
               (subvec line 10)))
     (filter (comp-> count #{2 4 3 7}))
     count)

;; Part 2
(defn solve [line]
  (let [digits (subvec line 0 10)
        secrets (subvec line 10)
        count->digits (group-by count (mapv set digits))
        mapping {1 (-> 2 count->digits first)
                 7 (-> 3 count->digits first)
                 4 (-> 4 count->digits first)
                 8 (-> 7 count->digits first)}
        mapping (assoc mapping 3 (-> 5 count->digits
                                     (->> (filter #(set/superset? % (mapping 7))))
                                     first))
        mapping (assoc mapping 2 (-> 5 count->digits
                                     (->> (remove #{(mapping 3)}))
                                     (->> (filter #(= (count (set/intersection % (mapping 4))) 2)))
                                     first))
        mapping (assoc mapping 5 (-> 5 count->digits
                                     (->> (remove #{(mapping 3) (mapping 2)}))
                                     first))
        mapping (assoc mapping 9 (-> 6 count->digits
                                     (->> (filter #(set/superset? % (mapping 4))))
                                     first))
        mapping (assoc mapping 0 (-> 6 count->digits
                                     (->> (remove #{(mapping 9)}))
                                     (->> (filter #(set/superset? % (mapping 1))))
                                     first))
        mapping (assoc mapping 6 (-> 6 count->digits
                                     (->> (remove #{(mapping 9) (mapping 0)}))
                                     first))
        rmapping (into {} (map (fn [[k v]] [v k])) mapping)]
    (->> secrets
         (map set)
         (map rmapping)
         (apply str)
         parse-long)))

(transduce (map solve) + input)
; => 936117
