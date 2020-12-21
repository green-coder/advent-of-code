(ns aoc.day-21
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (->> (str/split-lines input-str)
       (mapv (fn [line]
               (let [[_ ingredients allergens] (re-find #"([^(]*) \(contains ([^)]*)\)" line)]
                 [(set (re-seq #"\w+" ingredients))
                  (set (re-seq #"\w+" allergens))])))))

;; Demo input
(def input
  (parse-input "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"))

;; Real input
(def input
  (parse-input (slurp (io/resource "day21.txt"))))


;; Part 1
(def dangerous-ingredients
  (let [allergens (->> input
                       (mapcat second)
                       set
                       vec)]
    (->> (for [a allergens]
           (->> input
                (filter (comp-> second #(contains? % a)))
                (map first)
                (apply set/intersection)))
         (apply concat)
         set)))

(->> input
     (map first)
     (apply concat)
     (remove dangerous-ingredients)
     count)
;=> 2282


;; Part 2

(let [allergens (->> input
                     (mapcat second)
                     set
                     vec)]
  (->> (for [a allergens]
         [a (->> input
                 (filter (comp-> second #(contains? % a)))
                 (map first)
                 (apply set/intersection))])
       (sort-by (comp-> second count))))

;; ~~~
;; Solved by hand from the value returned by the expression above.
;; ~~~

;; Formatting for submitting the answer.
(->> '(["shellfish" "rkzqs"]
       ["sesame" "ctmzsr"]
       ["nuts" "mbdksj"]
       ["fish" "hphcb"]
       ["peanuts" "vzzxl"]
       ["soy" "zmhnj"]
       ["dairy" "vrzkz"]
       ["eggs" "zjsh"])
      (sort-by first)
      (map second)
      (str/join ","))
;=> "vrzkz,zjsh,hphcb,mbdksj,vzzxl,ctmzsr,rkzqs,zmhnj"

