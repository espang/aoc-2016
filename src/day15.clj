(ns day15)

(defn disc [size start]
  (map-indexed vector (drop start (cycle (range size)))))

(defn ok? [elements]
  (every? #(zero? (second %)) elements))

(defn solve [& discs]
  (->> (apply map vector discs)
       (filter ok?)
       first
       first
       first
       dec))

(comment
  (solve (disc 5 4)
         (disc 2 (inc 1)))
  (time (solve (disc 13 10)
               (disc 17 16)
               (disc 19 0)
               (disc 7 4)
               (disc 5 4)
               (disc 3 6)))
  (time (solve (disc 13 10)
               (disc 17 16)
               (disc 19 0)
               (disc 7 4)
               (disc 5 4)
               (disc 3 6)
               (disc 11 6)))
  ,)
