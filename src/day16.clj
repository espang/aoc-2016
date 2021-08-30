(ns day16)

(defn step [a]
  (concat a
          [\0]
          (map (fn [c] (case c \1 \0 \0 \1))
               (reverse a))))

(defn checksum [a]
  (let [a' (->> a
                (partition 2)
                (map (fn [[c1 c2]] (if (= c1 c2) 1 0))))]
    (if (zero? (mod (count a') 2))
      (recur a')
      a')))

(defn solve [a size]
  (loop [a a]
    (if (>= (count a) size)
      (apply str (checksum (take size a)))
      (recur (step a)))))

(comment
  (-> (step "1")
      step)
  (apply str (step "111100001010"))
  (checksum "10000011110010000111")
  (solve "10011111011011001" 272)
  ; takes ~100s
  (time (solve "10011111011011001" 35651584)))
