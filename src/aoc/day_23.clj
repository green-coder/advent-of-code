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
(def input-count (count input))

(defn index->label [index]
  (if (< index input-count)
    (input index)
    (inc index)))

(defn label->index [label]
  (if (<= label input-count)
    (loop [index 0]
      (if (= (index->label index) label)
        index
        (recur (inc index))))
    (dec label)))

(defn play-game [nb-cups nb-rounds]
  (let [next-index (into []
                         (map (fn [index]
                                (mod (inc index) nb-cups)))
                         (range nb-cups))
        lower-label (fn [x]
                      (-> x (- 2) (mod nb-cups) inc))
        fast-game-round (fn [[current-index next-index]]
                          (let [picked-up1-index (next-index current-index)
                                picked-up2-index (next-index picked-up1-index)
                                picked-up3-index (next-index picked-up2-index)
                                dest-label (->> (lower-label (index->label current-index))
                                                (iterate lower-label)
                                                (remove #{(index->label picked-up1-index)
                                                          (index->label picked-up2-index)
                                                          (index->label picked-up3-index)})
                                                first)
                                dest-index (label->index dest-label)
                                next-index (assoc next-index
                                             ;; set the current's next
                                             current-index (next-index picked-up3-index)
                                             ;; set the dest's next
                                             dest-index picked-up1-index
                                             ;; set the last pickup's next
                                             picked-up3-index (next-index dest-index))]
                            [(next-index current-index) next-index]))]
    (-> (iterate fast-game-round [0 next-index])
        (nth nb-rounds))))

#_(let [[current-index next-index] (play-game input-count 100)]
    (->> (label->index 1)
         (iterate next-index)
         (drop 1)
         (map index->label)
         (take-while (complement #{1}))
         (apply str)))
;=> "76952348"

;; Part 2
#_ (time (let [[current-index next-index] (play-game 1000000 10000000)]
           (* (-> 1 label->index next-index index->label)
              (-> 1 label->index next-index next-index index->label))))
;=> 72772522064
