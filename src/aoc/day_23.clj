(ns aoc.day-23
  (:refer-clojure :exclude [group-by])
  (:require [aoc.util :refer :all]))

(defn parse-input [input-str]
  (mapv (comp-> str parse-number) input-str))

;; Demo input
(def input (parse-input "389125467"))

; Real input
(def input (parse-input "653427918"))


;; Part 1
(defn remove-cups [cups]
  (into [(first cups)] (subvec cups 4)))

(defn insert-cups [cups index elms]
  (vec (concat (subvec cups 0 index) elms (subvec cups index))))

(defn rotate-cups [cups]
  (conj (subvec cups 1) (first cups)))

(defn game-move [cups]
  (let [current (first cups)
        pickup-cups (subvec cups 1 4)
        cups (remove-cups cups)
        destination (->> current
                         (iterate (fn [x]
                                    (inc (mod (- x 2) (count input)))))
                         (drop 1)
                         (remove (set pickup-cups))
                         first)
        dest-index (->> cups
                        (keep-indexed (fn [index v]
                                        (when (= v destination)
                                          index)))
                        first)
        cups (insert-cups cups (inc dest-index) pickup-cups)]
    (rotate-cups cups)))

;; Some "by hand" data process was happening here ...
#_ (->> input
        (iterate game-move)
        (drop 100)
        first
        rotate-cups
        rotate-cups
        rotate-cups
        rotate-cups
        rotate-cups
        rotate-cups
        rotate-cups)


;; Part 2

(def nb-cups 1000000)
(def cups (into input (range 10 (inc nb-cups))))
(def next-cup-index (into []
                          (map (fn [x]
                                 (mod x (count cups))))
                          (range 1 (inc nb-cups))))

(defn index-of [elm]
  (if (<= elm (count input))
    (loop [index 0]
      (if (= (cups index) elm)
        index
        (recur (inc index))))
    (dec elm)))

(defn fast-game-move [[current-cup-index next-cup-index]]
  (let [pickup-cup-indexes (->> current-cup-index
                                (iterate next-cup-index)
                                (drop 1)
                                (take 3))
        pickup-cup-labels (map cups pickup-cup-indexes)
        dest-label (->> (cups current-cup-index)
                        (iterate (fn [x]
                                   (inc (mod (- x 2) (count cups)))))
                        (drop 1)
                        (remove (set pickup-cup-labels))
                        first)
        dest-index (index-of dest-label)
        next-cup-index (assoc next-cup-index
                         ;; set the current's next
                         current-cup-index (next-cup-index (last pickup-cup-indexes))
                         ;; set the dest's next
                         dest-index (index-of (first pickup-cup-labels))
                         ;; set the last pickup's next
                         (last pickup-cup-indexes) (next-cup-index dest-index))]
    [(next-cup-index current-cup-index) next-cup-index]))

(let [[current-cup-index next-cup-index]
      (->> [0 next-cup-index]
           (iterate fast-game-move)
           (drop 10000000)
           first)]
  (* (cups (next-cup-index (index-of 1)))
     (cups (next-cup-index (next-cup-index (index-of 1))))))
;=> 72772522064
