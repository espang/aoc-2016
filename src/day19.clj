(ns day19
  (:require
   [clojure.data.finger-tree :as ft]))

(defprotocol INode
  (get-next [this])
  (update-next [this n])
  (get-prev [this])
  (update-prev [this n])
  (get-presents [this])
  (add-presents [this x]))

(deftype Node [^:unsynchronized-mutable prev
               ^:unsynchronized-mutable next
               ^:unsynchronized-mutable data
               pos]
  INode
  (get-next [this] next)
  (update-next [this n] (set! next n))
  (get-prev [this] prev)
  (update-prev [this n] (set! prev n))
  (get-presents [this] data)
  (add-presents [this x] (set! data (+ data x))))

(defn construct [n]
  (let [first (->Node nil nil 1 1)]
    (loop [last first
           cnt  1]
      (if (= cnt n)
        (do
          (.update-next last first)
          (.update-prev first last)
          first)
        (let [n (->Node last nil 1 (inc cnt))]
          (.update-next last n)
          (recur n (inc cnt)))))))

(defn steal-presents [node]
  (loop [stealer node]
    (if (= (.pos stealer) (.pos (.get-next stealer)))
      (.pos stealer)
      (let [next (.get-next stealer)]
        (.add-presents stealer (.get-presents next))
        (.update-next stealer (.get-next next))
        (recur (.get-next next))))))

(comment
  (steal-presents (construct 5))
  (steal-presents (construct 3018458))
  ,)

(defn forward-n [node n]
  (loop [node' node
         n     n]
    (if (zero? n)
      node'
      (recur (.get-next node') (dec n)))))

(defn steal-presents2 [n]
  (let [start (time (construct n))]
  (loop [stealer start
         size    n]
    (if (= 1 size)
      (.pos stealer)
      (let [next (forward-n stealer (quot size 2))]
        ;; update current elves number of presents
        (.add-presents stealer (.get-presents next))
        ;; remove next from the list
        (.update-next (.get-prev next) (.get-next next))
        (.update-prev (.get-next next) (.get-prev next))
        (recur (.get-next stealer) (dec size)))))))

(comment
  (steal-presents2 5)
  (steal-presents2 3018458)
  (time (forward-n (construct 3018458) (quot 3018458 2))))


(defn steal-presents2-ft [n]
  (let [start (time (apply ft/counted-double-list (drop 1 (map-indexed vector (repeat (inc n) 1)))))]
    (loop [i   0
           cdl start
           cnt n]
      ;(tap> [i (nth cdl i) cdl])
      (when (zero? (mod cnt 100000))
        (tap> (str cnt " elements left")))
      (if (= 1 cnt)
        cdl
        (let [fwd      (quot cnt 2)
              idx      (mod (+ i fwd) cnt)
              [l _ r]  (ft/ft-split-at cdl idx)
              next-i   (if (> i idx) i (inc i))]
          (recur (mod next-i (dec cnt))
                 (ft/ft-concat l r)
                 (dec cnt)))))))

(comment
  (steal-presents2-ft 5)
  (steal-presents2-ft 3018458))
