(ns day21)



(defn swap [v [_ x y]]
  (-> v
      (assoc x (v y))
      (assoc y (v x))))

(defn swap-letter [v [_ x y]]
  (replace {x y y x} v))

(defn rotate [v [_ dir n]]
  (case dir
    :left (let [v1 (into [] (subvec v 0 n))
                v2 (into [] (subvec v n (count v)))]
            (into v2 v1))
    :right (let [size (count v)
                 v1 (into [] (subvec v 0 (- size n)))
                 v2 (into [] (subvec v (- size n) size))]
             (into v2 v1))))

(defn rotate-letter [v [_ x]]
  (let [i (reduce (fn [acc x']
                    (if (= x x')
                      (reduced acc)
                      (inc acc)))
                  0
                  v)]
    (cond-> v
      true (rotate [nil :right 1])
      true (rotate [nil :right i])
      (>= i 4) (rotate [nil :right 1]))))

(defn reverse-xy [v [_ x y]]
  (let [v1 (into [] (subvec v 0 x))
        v2 (into [] (reverse (subvec v x (inc y))))
        v3 (into [] (subvec v (inc y)))]
    (vec (concat v1 v2 v3))))

(defn move [v [_ x y]]
  (cond
    (< x y)
    (let [v1 (into [] (subvec v 0 x))
          v2 (into [] (subvec v (inc x) (inc y)))
          v3 (into [] (subvec v (inc y)))]
      (-> v1
          (into v2)
          (conj (v x))
          (into v3)))

    (= x y) v

    (> x y)
    (let [v1 (into [] (subvec v 0 y))
          v2 (into [] (subvec v y x))
          v3 (into [] (subvec v (inc x)))]
      (-> v1
          (conj (v x))
          (into v2)
          (into v3)))))

(comment
  (swap (vec "abcd") 1 3)
  (swap-letter (vec "abcd") \b \d)
  (rotate (vec "abcd") :right 1)
  (rotate (vec "abcd") :left 1)
  (rotate-letter (vec "abcdef") \d)
  (reverse-xy (vec "abcdef") 2 3)
  (move (vec "abcdef") 1 4)

  (-> (vec "abcde")
      (swap 4 0)
      (swap-letter \d \b)
      (reverse-xy 0 4)
      (rotate :left 1)
      (move 1 4)
      (move 3 0)
      (rotate-letter \b)
      (rotate-letter \d)))

(defn parse-digits [s pattern]
  (when-let [[_ x y] (re-matches
                      pattern
                      s)]
    [(Integer/parseInt x)
     (Integer/parseInt y)]))

(defn parse-move [s]
  (when-let [[x y] (parse-digits s #"move position (\d) to position (\d)")]
    [:move x y]))

(defn parse-reverse [s]
  (when-let [[x y] (parse-digits s #"reverse positions (\d) through (\d)")]
    [:reverse-xy x y]))

(defn parse-swap [s]
  (when-let [[x y] (parse-digits s #"swap position (\d) with position (\d)")]
    [:swap x y]))

(defn parse-swap-letter [s]
  (when-let [[_ x y] (re-matches
                      #"swap letter (\w) with letter (\w)"
                      s)]
    [:swap-letter
     (first x)
     (first y)]))

(defn parse-rotate-letter [s]
  (when-let [[_ x] (re-matches
                    #"rotate based on position of letter (\w)"
                    s)]
    [:rotate-letter
     (first x)]))

(defn parse-rotate [s]
  (when-let [[_ x y _] (re-matches
                        #"rotate (left|right) (\d) (step|steps)"
                        s)]
    [:rotate
     (keyword x)
     (Integer/parseInt y)]))

(defn parse-line [l]
  (if-let [r (parse-move l)]
    r
    (if-let [r (parse-reverse l)]
      r
      (if-let [r (parse-swap l)]
        r
        (if-let [r (parse-swap-letter l)]
          r
          (if-let [r (parse-rotate l)]
            r
            (if-let [r (parse-rotate-letter l)]
              r
              nil)))))))

(def commands
  (->> "input_21.txt"
       slurp
       clojure.string/split-lines
       (map parse-line)))

(defn run [pw commands]
  (loop [commands (seq commands)
         v        (vec pw)]
    (if (seq commands)
      (let [cmd (first commands)
            v' (case (first cmd)
                 :swap (swap v cmd)
                 :move (move v cmd)
                 :swap-letter (swap-letter v cmd)
                 :rotate (rotate v cmd)
                 :rotate-letter (rotate-letter v cmd)
                 :reverse-xy (reverse-xy v cmd))]
        (recur (rest commands) v'))
      (apply str v))))

(comment
  (parse-move "move position 2 to position 6")
  (parse-swap "swap letter b with letter f")
  (run "abcdefgh" commands))

(def want "fbgdceah")

(defn permutations
  "calculates all permutations for distinct values.
   coll has to be a set."
  [coll]
  (if (= 1 (count coll))
    (list coll)
    (for [element coll
          perms   (permutations (disj coll element))]
      (cons element perms))))

(comment
  (->> want
       set
       permutations
       (map (fn [pw] [(apply str pw) (run pw commands)]))
       (filter (fn [[_ scrambled]] (= scrambled want)))
       first)
  ,)
