(ns day13)

(defn odd [n] (= 1 (mod n 2)))
(defn even [n] (= 0 (mod n 2)))

(defn bits-set [nbr]
  (loop [n   nbr
         cnt 0]
    (if (= 0 n)
      cnt
      (recur (bit-shift-right n 1)
             (+ cnt (bit-and n 1))))))

(defn is-space? [x y nbr]
  (even (bits-set (+ (+ (* x x)
                        (* 3 x)
                        (* 2 x y)
                        y
                        (* y y))
                     nbr))))

(defn row-y [size nbr y]
  (apply str (for [x (range size)] (if (is-space? x y nbr) \. \#))))

(defn spaces-around [x y nbr]
  (filter (fn [[x y]]
            (and (>= x 0)
                 (>= y 0)
                 (is-space? x y nbr)))
          (map (fn [[dx dy]]
                 [(+ x dx) (+ y dy)])
               [[1 0] [-1 0] [0 1] [0 -1]])))

(defn go-to [x y nbr]
  (loop [q (conj (clojure.lang.PersistentQueue/EMPTY) [0 1 1])
         v #{}]
    (if (empty? q)
      -1
      (let [[steps x' y'] (peek q)]
        (if (and (= x x') (= y y'))
          steps
          (let [next-steps (->> (spaces-around x' y' nbr)
                                (remove (fn [xy] (contains? v xy)))
                                (map (fn [[x y]] [(inc steps) x y])))]
            (recur (apply conj (pop q) next-steps)
                   (conj v [x' y']))))))))

(defn reachable [n nbr]
  (loop [q (conj (clojure.lang.PersistentQueue/EMPTY) [0 1 1])
         v #{}]
    (if (empty? q)
      -1
      (let [[steps x' y'] (peek q)]
        (if (> steps n)
          (count v)
          (let [next-steps (->> (spaces-around x' y' nbr)
                                (remove (fn [xy] (contains? v xy)))
                                (map (fn [[x y]] [(inc steps) x y])))]
            (recur (apply conj (pop q) next-steps)
                   (conj v [x' y']))))))))

(comment
  (go-to 7 4 10)
  (go-to 31 39 1350)
  (reachable 50 1350))