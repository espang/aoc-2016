(ns day23-quick
  (:require
   [clj-async-profiler.core :as prof]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

;; register is a long array. Functions
;; to interact with the register:

(defn inc-r [^longs r i]
  (aset r i (inc (aget r i))))

(defn dec-r [^longs r i]
  (aset r i (dec (aget r i))))

(defn set-r [^longs r i ^long v]
  (aset r i v))

(defn get-r [^longs r i]
  (aget r i))

;; Parsing the input
(defn parse-val [s]
  (case s
    "a" [0 true]
    "b" [1 true]
    "c" [2 true]
    "d" [3 true]
    [(Long/parseLong s) false]))

(defn parse-op [s]
  (case s
    "inc" :inc
    "dec" :dec
    "tgl" :toggle
    "cpy" :copy
    "jnz" :jump))

(defprotocol ToggleOP
  (getO [this])
  (setO [this o])
  (printE [this]))

(deftype Expr [^:unsynchronized-mutable o
               x-val
               x-is-loc
               y-val
               y-is-loc]
  ToggleOP
  (getO [this] o)
  (setO [this no] (set! o no))
  (printE [this]
          (println "Expr(" (name o) ") " x-is-loc ":" x-val "|" y-is-loc ":" y-val)))

(defn parse-line [line]
  (let [[o x y] (str/split line #" ")
        o (parse-op o)
        [x xl] (parse-val x)
        [y yl] (when y (parse-val y))]
    (->Expr o x xl y yl)))

(defn parse-file [filename]
  (->> filename
       slurp
       str/split-lines
       (mapv parse-line)))

(defn execute [^Expr expr reg i exprs]
  (let [o (getO expr)]
    (case o
      :inc
      (do (inc-r reg (.x-val expr))
          (inc i))

      :dec
      (do (dec-r reg (.x-val expr))
          (inc i))

      :toggle
      (let [change-at (+ i (get-r reg (.x-val expr)))]
        (when (< change-at (alength exprs))
          (let [change-expr (aget exprs change-at)
                change-o    (getO change-expr)]
            (setO change-expr
                  (case change-o
                    :inc :dec
                    :dec :inc
                    :toggle :inc
                    :copy :jump
                    :jump :copy))))
        (inc i))

      :copy
      (do
        (when (.y-is-loc expr)
          (set-r reg
                 (.y-val expr)
                 (if (.x-is-loc expr)
                   (get-r reg (.x-val expr))
                   (.x-val expr))))
        (inc i))

      :jump
      (let [v (if (.x-is-loc expr) (get-r reg (.x-val expr)) (.x-val expr))]
        (if-not (zero? v)
          (+ i (if (.y-is-loc expr) (get-r reg (.y-val expr)) (.y-val expr)))
          (inc i))))))

(defn solve [a ^objects exprs]
  (let [r         (long-array 4)
        max-index (alength exprs)]()
    (set-r r 0 a)
    (loop [i 0]
      (when (< i max-index)
        (recur (execute (aget exprs i)
                        r
                        i
                        exprs))))
    (get-r r 0)))

(comment
  (do
    (def exprs (to-array (parse-file "input_23.txt")))
    (prof/start)
    (time (solve 11 exprs))
    (prof/stop))

  (do
    (def exprs (to-array (parse-file "input_23.txt")))
    (time (solve 12 exprs)))

  (prof/serve-files 8082)
  (double (/ 124 23)))