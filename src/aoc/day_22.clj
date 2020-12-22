(ns aoc.day-22
  (:refer-clojure :exclude [group-by])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.data.priority-map :as pm]
            [clojure.math.combinatorics :as comb]
            [medley.core :as medley]
            [aoc.util :refer :all]))

(defn parse-input [input-str]
  (->> (str/split input-str #"\R\R")
       (mapv (fn [deck]
               (let [[_ & cards] (str/split-lines deck)]
                 (mapv parse-number cards))))))

;; Demo input
(def input (parse-input "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"))


; Real input
(def input
  (parse-input (slurp (io/resource "day22.txt"))))


;; Part 1
(defn play-game [deck1 deck2]
  (cond (empty? deck1) deck2
        (empty? deck2) deck1
        :else
        (let [c1 (first deck1)
              c2 (first deck2)]
          (if (< c1 c2)
            (recur (subvec deck1 1)
                   (conj (subvec deck2 1) c2 c1))
            (recur (conj (subvec deck1 1) c1 c2)
                   (subvec deck2 1))))))

(defn score [deck]
  (apply + (map * deck (range (count deck) 0 -1))))

(score (play-game (first input) (second input)))


;; Part 2
(defn play-rec-game [seen-decks deck1 deck2]
  (cond (seen-decks [deck1 deck2]) [1 deck1]
        (empty? deck1) [2 deck2]
        (empty? deck2) [1 deck1]
        :else
        (let [c1 (first deck1)
              c2 (first deck2)
              seen-decks (conj seen-decks [deck1 deck2])
              player1-win? (if (and (> (count deck1) c1)
                                    (> (count deck2) c2))
                             (= 1 (first (play-rec-game #{}
                                                        (subvec deck1 1 (+ 1 c1))
                                                        (subvec deck2 1 (+ 1 c2)))))
                             (> c1 c2))]
          (if player1-win?
            (recur seen-decks
                   (conj (subvec deck1 1) c1 c2)
                   (subvec deck2 1))
            (recur seen-decks
                   (subvec deck1 1)
                   (conj (subvec deck2 1) c2 c1))))))

(score (second (play-rec-game #{}
                              (first input)
                              (second input))))
