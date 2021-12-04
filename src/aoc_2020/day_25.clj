(ns aoc-2020.day-25)


(defn stepper [subject-number]
  (fn [x] (mod (* x subject-number) 20201227)))

(defn find-loop-size [subject-number public-key]
  (->> (iterate (stepper subject-number) 1)
       (keep-indexed (fn [index x]
                       (when (= x public-key)
                         index)))
       first))


;; Demo input
(find-loop-size 7 5764801) ; 8
(find-loop-size 7 17807724) ; 11

; Demo solution
(-> (iterate (stepper 17807724) 1)
    (nth 8))
;=> 14897079


;; Real input
(find-loop-size 7 8252394) ; 18739604
(find-loop-size 7 6269621) ; 5166072

; Real solution
(-> (iterate (stepper 6269621) 1)
    (nth 18739604))
;=> 181800
