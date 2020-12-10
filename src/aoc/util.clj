(ns aoc.util
  (:refer-clojure :exclude [group-by])
  (:require [clojure.core :as cc]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn parse-number [s]
  (edn/read-string s))

; Reads only the first expr.
#_ (parse-number "\n 18 15 \n")

; Reads ANY edn expression, not just numbers.
#_ (parse-number "\n \"18\" 15 \n")

(defn parse-binary [s]
  (edn/read-string (str "2r" (str/triml s))))

(defn parse-octal [s]
  (edn/read-string (str "0" (str/triml s))))

(defn parse-hexa [s]
  (edn/read-string (str "0x" (str/triml s))))

#_ (str/triml " \n caFE \n ")
#_ (parse-hexa " \n caFE \n ")

(defn parse-base-n [n s]
  (edn/read-string (str n "r" (str/triml s))))

#_ (parse-base-n 36 "10") ; [0-9a-zA-Z]*


(defmacro first-for [& for-args]
  `(first (for ~@for-args)))


(defn comp-> [& args]
  (apply comp (reverse args)))

#_ ((comp str inc) 17) ; => "18"
#_ ((comp-> inc str) 17) ; => "18"


(defn group-by
  "Same as clojure.core/group-by, but with some handy new arities which apply
   custom map & reduce operations to the elements grouped together under the same key."
  ([kf coll]
   ;(group-by kf identity conj [] coll)
   (cc/group-by kf coll))
  ([kf vf coll]
   (group-by kf vf conj [] coll))
  ([kf vf rf coll]
   (group-by kf vf rf (rf) coll))
  ([kf vf rf init coll]
   (persistent!
    (reduce
     (fn [ret x]
       (let [k (kf x)
             v (vf x)]
         (assoc! ret k (rf (get ret k init) v))))
     (transient {}) coll))))

#_ (group-by first [[:a 1] [:a 2] [:b 3] [:a 4] [:b 5]])
#_ (group-by first second [[:a 1] [:a 2] [:b 3] [:a 4] [:b 5]])
#_ (group-by first second + [[:a 1] [:a 2] [:b 3] [:a 4] [:b 5]])
#_ (group-by first second + 10 [[:a 1] [:a 2] [:b 3] [:a 4] [:b 5]])


(defn partitionv
  ([n vec-coll]
   (partitionv n n vec-coll))
  ([n step vec-coll]
   (for [i (range n (inc (count vec-coll)) step)]
     (subvec vec-coll (- i n) i))))

#_ (partitionv 3 (vec (range 10)))
#_ (partitionv 3 1 (vec (range 10)))


(defn partitionv-all
  [n vec-coll]
  (let [coll-size (count vec-coll)]
    (for [i (range 0 coll-size n)]
      (subvec vec-coll i (min (+ i n) coll-size)))))

#_ (partitionv-all 3 (vec (range 10)))


(defn partition-indexes
  "Returns pairs of indexes [min sup] to be used with subvec,
   where min is included and sup is excluded."
  ([n coll-size]
   (partition-indexes n n coll-size))
  ([n step coll-size]
   (for [i (range n (inc coll-size) step)]
     [(- i n) i])))

#_ (partition-indexes 3 10)
#_ (partition-indexes 3 1 10)


(defn partition-all-indexes
  "Returns pairs of indexes [min sup] to be used with subvec,
   where min is included and sup is excluded."
  [n coll-size]
  (for [i (range 0 coll-size n)]
    [i (min (+ i n) coll-size)]))

#_ (partition-all-indexes 3 10)
