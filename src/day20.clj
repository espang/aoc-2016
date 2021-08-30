(ns day20)

(def ranges
  (->> (slurp "input_20.txt")
       clojure.string/split-lines
       (map #(mapv (fn [x] (Long/parseLong x)) (clojure.string/split % #"-")))
       sort))

(defn solve [ranges]
  (reduce (fn [[total u1] [l2 u2]]
            (if (>= (inc u1) l2)
              [total (max u1 u2)]
              [(+ total (- l2 (inc u1))) u2]))
          [0 0]
          ranges))

(comment
  (solve [[0 2] [4 5] [7 9]])
  (solve ranges)
  ,)