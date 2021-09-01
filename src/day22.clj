(ns day22)

(defn parse-name [n]
  (let [[_ x y] (clojure.string/split n #"-")]
    [(Integer/parseInt (apply str (rest x)))
     (Integer/parseInt (apply str (rest y)))]))

(defn to-nbr [x]
  (Integer/parseInt
   (apply str (take (dec (count x)) x))))

(def nodes
  ;; manually dropped the first 2 lines of the file
  (->> "input_22.txt"
       slurp
       clojure.string/split-lines
       (map (fn [line]
              (remove empty? (clojure.string/split line #" "))))
       (map (fn [[node size used available use%]]
              [(parse-name node)
               (to-nbr size)
               (to-nbr used)
               (to-nbr available)
               (to-nbr use%)]))))

(defn viable-pair [[[x1 y1] _ u1 a1 _] [[x2 y2] _ u2 a2 _]]
  (when (not= [x1 y1] [x2 y2])
    (or (and (not= 0 u1) (<= u1 a2))
        (and (not= 0 u2) (<= u2 a1)))))

(defn check-from [n i v]
  (reduce (fn [acc [i node]]
            (if (viable-pair n node)
              (inc acc)
              acc))
          0
          (subvec v i)))

(defn all-viable-pairs [nodes]
  (let [inodes (vec (map-indexed vector nodes))]
    (reduce (fn[acc [i node]]
              (+ acc (check-from node i inodes)))
            0
            inodes)))

(comment
  (all-viable-pairs nodes))