(ns aoc.util
  (:require [clojure.edn :as edn]
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

