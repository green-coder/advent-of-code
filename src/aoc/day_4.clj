(ns aoc.day-4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            #_[clojure.core.logic :as logic]
            [medley.core :as medley]
            #_[ubergraph.core :as uber]
            [minimallist.core :as mini]
            [minimallist.helper :as h]
            #_[diffuse.core :as diff]
            #_[diffuse.helper :as dh]
            [lambdaisland.regal :as regal]))

;; Inspired from my blog post on transducers:
;; https://vincent.404.taipei/clojure/build-your-own-transducer-part3/
(defn partition-around [separator?]
  (fn [rf]
    (let [state (volatile! [])]
      (fn ([] (rf))
          ([result] (-> result
                      (rf @state)
                      (rf)))
          ([result input]
           (let [buffered-elements @state]
             (if (separator? input)
               (do (vreset! state [])
                   (rf result buffered-elements))
               (do (vreset! state (conj buffered-elements input))
                   result))))))))

(def input
  (->> (line-seq (io/reader (io/resource "day4.txt")))
       (sequence (partition-around str/blank?))
       (map (partial str/join " "))
       (map (fn [passport]
              (->> (str/split passport #" ")
                   (into {}
                         (map (fn [part]
                                (let [[_ k v] (re-find #"([^:]+):(.*)" part)]
                                  [k v])))))))))

(def required-fields
  #{"byr"
    "iyr"
    "eyr"
    "hgt"
    "hcl"
    "ecl"
    "pid"
    #_"cid"})


;; Part 1
(->> input
     (filter (fn [passport]
               (let [fields (set (keys passport))]
                 (empty? (set/difference required-fields fields)))))
     count)


;; Part 2 - let's just use Minimallist with Regal, just for fun.
(def passport-model
  (let [re-matches (fn [regal-expr text]
                     (re-matches (regal/regex regal-expr) text))
        four-digits-between (fn [min max]
                              (h/fn (fn [x]
                                      (when-let [n (re-matches [:repeat :digit 4] x)]
                                        (<= min (Long/parseLong n) max)))))]
    (-> (h/map
          ["byr" (four-digits-between 1920 2002)]
          ["iyr" (four-digits-between 2010 2020)]
          ["eyr" (four-digits-between 2020 2030)]
          ["hgt" (h/fn (fn [x]
                         (when-let [[_ n unit] (re-matches [:cat
                                                            [:capture [:+ :digit]]
                                                            [:capture [:alt "cm" "in"]]]
                                                           x)]
                           (let [n (Long/parseLong n)]
                             (case unit
                               "cm" (<= 150 n 193)
                               "in" (<= 59 n 79)
                               false)))))]
          ["hcl" (h/fn (partial re-matches [:cat "#" [:repeat [:class ["0" "9"] ["a" "f"]] 6]]))]
          ["ecl" (h/enum #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})]
          ["pid" (h/fn (partial re-matches [:repeat :digit 9]))])
      (h/with-optional-entries ["cid" (h/fn any?)]))))

(->> input
     (filter (partial mini/valid? passport-model))
     count)
