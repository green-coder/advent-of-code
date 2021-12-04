(ns aoc-2020.day-19
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [instaparse.transform :as insta-tf]
            [aoc.util :refer :all]))

(def input (slurp (io/resource "2020/day19.txt")))

(def input-parser (insta/parser "
  <input> = rules <#'\\R\\R'> messages
  rules = rule+
  messages = message+
  rule = rule-ref <':'> disjunction
  disjunction = conjunction (<'|'> conjunction)*
  conjunction = rule-ref+ | text-literal
  rule-ref = #'\\d+'
  text-literal = <'\"'> #'[a-z]+' <'\"'>
  <message> = #'.+'
" :auto-whitespace :standard))

(let [[rules messages] (->> (input-parser input)
                            (insta-tf/transform {:rules (fn [& rules] (vec rules))
                                                 :messages (fn [& messages] (vec messages))
                                                 :rule (fn [rule-id rule-definition]
                                                         (str rule-id " = " rule-definition "\n"))
                                                 :rule-ref (fn [rule-id]
                                                             (str "rule" rule-id))
                                                 :disjunction (fn [& args]
                                                                (str/join " | " args))
                                                 :conjunction (fn [& args]
                                                                (str/join " " args))
                                                 :text-literal (fn [text-literal]
                                                                 (str "'" text-literal "'"))}))
      new-grammar (apply str rules)
      new-parser (insta/parser new-grammar :start :rule0)]
  (->> messages
       (map (partial insta/parses new-parser))
       (remove insta/failure?)
       count))
