(ns aoc-2020.day-4
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
            #_[lambdaisland.regal :as regal]))

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
  (->> (line-seq (io/reader (io/resource "2020/day4.txt")))
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


;; Part 2 - let's just use Minimallist.
(def passport-model
  (let [digit-model (h/char-set "0123456789")
        hexa-model (h/char-set "0123456789abcdef")
        four-digits-between (fn [min max]
                              (h/and (h/in-string (h/repeat 4 4 digit-model))
                                     (h/fn (fn [x]
                                             (<= min (Long/parseLong x) max)))))]
    (-> (h/map
          ["byr" (four-digits-between 1920 2002)]
          ["iyr" (four-digits-between 2010 2020)]
          ["eyr" (four-digits-between 2020 2030)]
          ["hgt" (-> (h/cat [:digit (h/+ digit-model)]
                            [:unit (h/alt [:cm (h/char-cat "cm")]
                                          [:in (h/char-cat "in")])])
                     h/in-string
                     ;; TODO: need to improve access to parsed data.
                     (h/with-condition (h/fn (fn [x]
                                               (let [size (count x)
                                                     n (Long/parseLong (subs x 0 (- size 2)))
                                                     unit (subs x (- size 2))]
                                                 (case unit
                                                   "cm" (<= 150 n 193)
                                                   "in" (<= 59 n 79)))))))]
          ["hcl" (h/in-string (h/cat (h/val \#) (h/repeat 6 6 hexa-model)))]
          ["ecl" (h/enum #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})]
          ["pid" (h/in-string (h/repeat 9 9 digit-model))])
      (h/with-optional-entries ["cid" (h/fn any?)]))))

(->> input
     (filter (partial mini/valid? passport-model))
     count)
