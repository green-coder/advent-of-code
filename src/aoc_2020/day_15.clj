(ns aoc-2020.day-15
  (:refer-clojure :exclude [group-by])
  (:require [clojure.string :as str]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (->> (str/split input-str #",")
       (mapv parse-number)))

(def input
  (parse-input "0,3,1,6,7,5"))

;; Part 1
(defn solve1 [input nb-turns]
  (loop [turn (inc (count input))
         last-spoken (last input)
         mem (into {}
                   (map-indexed (fn [index n]
                                  [n (inc index)]))
                   (butlast input))]
    (let [n (if (contains? mem last-spoken)
              (- turn (mem last-spoken) 1)
              0)]
      (if (= turn nb-turns)
        n
        (recur (inc turn) n (assoc mem last-spoken (dec turn)))))))

(solve1 input 2020)
;=> 852

;; Part 2
(solve1 input 30000000)
;=> 6007666
