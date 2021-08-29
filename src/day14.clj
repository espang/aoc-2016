(ns day14)

(set! *warn-on-reflection* true)

(defn md5 [^String s]
  (apply str
         (map (partial format "%02x")
              (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                         .reset
                         (.update (.getBytes s)))))))

(defn md52 [^String s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

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

(comment
  (time (md52 "abc0"))
  (time (md5 "abc0"))
  (time (do-md5 2016 "abc0"))
  (time (stretch-md5 2016 "abc0")))

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

(def elements (seq "abcdef0123456789"))

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
      (md52 s)
      (recur (inc n) (md5 s)))))

(defn find-all-quintuples [s]
  (loop [current    (first s)
         s          (rest s)
         cnt        1
         quintuples #{}]
    (let [quintuples' (if (= 5 cnt) (conj quintuples current) quintuples)]
      (if (empty? s)
        quintuples'
        (let [cnt' (if (= current (first s)) (inc cnt) 1)]
          (recur (first s) (rest s) cnt' quintuples'))))))


(defn find-key [hashes index c]
  (loop [i    (dec index)
         keys #{}]
    (if (or (<= i 0) (< i (- index 1000)))
      keys
      (if-let [{:keys [triple]} (hashes i)]
        (recur (dec i) (if (= triple c) (conj keys i) keys))
        (recur (dec i) keys)))))

(defn el64 [ss]
  (first (drop 63 ss)))

(defn find-nth2 [n salt]
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

(defn make-hashes [n salt]
  (into {} (for [i (range n)]
             [i (do-md5 2016 (str salt i))])))

(comment
  ;part 2
  ;; strech-md5: 40 secs to create 1000 hashes. Need ~23000 -> 8 minutes
  ;; do-md5: 4 secs to create 1000 hashes
  (time (def htabc (make-hashes 1000 "abc")))
  (stretch-md5 2016 "abc0")
  (find-all-quintuples "baaaacccccdeeeee")
  (time (def keys-test (find-nth2 64 "abc")))
  (el64 keys-test)
  (time (def keys-p2 (find-nth2 64 "ahsbgdzn")))
  (el64 keys-p2)
  ,)