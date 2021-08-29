(ns day14)

(set! *warn-on-reflection* true)

(defn md5 [^String s]
  (apply str
         (map (partial format "%02x")
              (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                         .reset
                         (.update (.getBytes s)))))))

(defn first-triple [s]
  (loop [current (first s)
         s       (rest s)
         cnt     1]
    (if (= 3 cnt)
      current
      (if (empty? s)
        nil
        (if (= current (first s))
          (recur current (rest s) (inc cnt))
          (recur (first s) (rest s) 1))))))

(defn contains-5 [s c]
  (loop [current (first s)
         s       (rest s)
         cnt     1]
    (if (and (= 5 cnt) (= current c))
      true
      (if (empty? s)
        false
        (if (= current (first s))
          (recur current (rest s) (inc cnt))
          (recur (first s) (rest s) 1))))))

(defn find-quintuple [index salt c]
  (loop [i index]
    (if (>= i (+ index 1000))
      false
      (if (contains-5 (md5 (str salt i)) c)
        true
        (recur (inc i))))))

(defn find-nth [n salt]
  (loop [index 0
         keys  []]
    (if (= (count keys) n)
      keys
      (let [hash (md5 (str salt index))]
        (if-let [c (first-triple hash)]
          (if (find-quintuple (inc index) salt c)
            (recur (inc index) (conj keys index))
            (recur (inc index) keys))
          (recur (inc index) keys))))))

(comment
  (find-nth 64 "abc")
  (find-nth 64 "ahsbgdzn"))

(defn stretch-md5 [times s]
  (loop [n 0
         s s]
    (if (= n times)
      (md5 s)
      (recur (inc n) (md5 s)))))

(defn do-md5 [times ^String s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        val       (format "%032x" (BigInteger. 1
                                               (.digest algorithm (.getBytes s))))]
    (loop [v val
           i 0]
      (if (= i times)
        v
        (recur (format "%032x" (BigInteger. 1
                                            (.digest (doto algorithm
                                                       .reset
                                                       (.update (.getBytes v))))))
               (inc i))))))


(defn find-all-quintuples
  "Finds all characters that occur at least 5 times in a row
  in the given string. Returns a set with all characters found
  or an empty set if none such character exist."
  [s]
  (loop [current    (first s)
         s          (rest s)
         cnt        1
         quintuples #{}]
    (let [quintuples' (if (= 5 cnt) (conj quintuples current) quintuples)]
      (if (empty? s)
        quintuples'
        (let [cnt' (if (= current (first s)) (inc cnt) 1)]
          (recur (first s) (rest s) cnt' quintuples'))))))

(defn find-key
  "Returns all indexes of hashes that have a first tripe of
  c in the last 1000 indexes. Looks up such triples in the
  hashes map."
  [hashes index c]
  (loop [i    (dec index)
         keys #{}]
    (if (or (<= i 0) (< i (- index 1000)))
      keys
      (if-let [{:keys [triple]} (hashes i)]
        (recur (dec i) (if (= triple c) (conj keys i) keys))
        (recur (dec i) keys)))))

(defn el64
  "returns the 64th element of a sorted-set"
  [ss]
  (first (drop 63 ss)))

(defn find-nth2
  "Searches for all keys defined by the rules of day14 y2016.
  It first find the first triple and memoizes those hashes.
  In case a triple exists it searches for all quintuples.
  For each quintuples it searches in the memoized triples if
  any valid keys can be found.
  It can stop once it reaches an index that is 1000 bigger than
  the 'n'th"
  [n salt]
  (loop [hashes {}
         index  0
         keys   (sorted-set)]
    (when (zero? (mod index 1000))
      (tap> (str "index: " index)))
    (if (and (>= (count keys) n)
             (<= (el64 keys) (- index 1000)))
      keys
      (let [hash       (do-md5 2016 (str salt index))
            ;; memoize the previous hashes
            hashes'    (if-let [t (first-triple hash)]
                         (assoc hashes index {:hash   hash
                                              :triple t})
                         hashes)
            quintuples (find-all-quintuples hash)
            keys'      (into #{} (mapcat #(find-key hashes (dec index) %) quintuples))
            keys'      (apply conj keys (disj keys' -1))]
        (recur hashes' (inc index) keys')))))

(defn make-hashes
  "creates a table of n hashes for the given salt."
  [n salt]
  (into {} (for [i (range n)]
             [i (do-md5 2016 (str salt i))])))

(comment
  ;part 2
  ;; strech-md5: 40 secs to create 1000 hashes. ~920s for 23k
  ;; do-md5:      4 secs to create 1000 hashes. ~ 92s for 23k
  (time (def htabc (make-hashes 1000 "abc")))
  (stretch-md5 2016 "abc0")
  (find-all-quintuples "baaaacccccdeeeee")
  (time (def keys-test (find-nth2 64 "abc")))
  (el64 keys-test)
  ; 98649.082242 msecs (~94% of the time is spend in creating the hashes)
  (time (def keys-p2 (find-nth2 64 "ahsbgdzn")))
  (el64 keys-p2)
  ,)