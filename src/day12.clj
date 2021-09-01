(ns day12)

(def register
  {\a 0
   \b 0
   \c 0
   \d 0})

(defn register? [s]
  (when (= 1 (count s))
    (#{\a \b \c \d} (get s 0))))

(defn to-nbr [s]
  (Integer/parseInt s))

(defn copy [register x y]
  (if (register? x)
    (assoc register y (register (get x 0)))
    (assoc register y (to-nbr x))))

(defn incr [register x]
  (update register x inc))

(defn decr [register x]
  (update register x dec))

(defn jump [register index x y]
  (let [v (if (register? x)
            (register (get x 0))
            (to-nbr x))]
    (if (zero? v)
      (inc index)
      (+ index y))))

(defn parse-line [line]
  (cond
    (clojure.string/starts-with? line "inc")
    (let [[_ s] (clojure.string/split line #" ")
          c     (get s 0)]
      (fn [register index]
        [(incr register c)
         (inc index)]))

    (clojure.string/starts-with? line "dec")
    (let [[_ s] (clojure.string/split line #" ")
          c     (get s 0)]
      (fn [register index]
        [(decr register c)
         (inc index)]))

    (clojure.string/starts-with? line "cpy")
    (let [[_ s1 s2] (clojure.string/split line #" ")
          c2     (get s2 0)]
      (fn [register index]
        [(copy register s1 c2)
         (inc index)]))

    (clojure.string/starts-with? line "jnz")
    (let [[_ s1 s2] (clojure.string/split line #" ")]
      (fn [register index]
        [register
         (jump register
               index
               s1
               (Integer/parseInt s2))]))))

(def fns (->> "input_12.txt"
              slurp
              clojure.string/split-lines
              (remove empty?)
              (mapv parse-line)))

(defn result [register fns]
  (loop [i 0
         r register]
    (if (>= i (count fns))
      r
      (let [f       (get fns i)
            [r' i'] (f r i)]
        (recur i' r')))))

(comment
  (incr register \a)
  (-> (copy register 10 \a)
      (copy \a \b))
  ((first fns) register 0)
  ((get fns 1) register 1)
  (result register fns)
  (result (assoc register \c 1) fns))