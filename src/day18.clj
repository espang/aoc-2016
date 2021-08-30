(ns day18)

(defn is-trap [l c r]
  (if (= \^ l)
    ;; Only its left tile is a trap.
    (or (= \. c r)
        (and (= \. c) (nil? r))
    ;; Its left and center tiles are traps, but its right tile is not.
        (and (= \^ c) (or (= \. r) (nil? r))))
    ;; Its center and right tiles are traps, but its left tile is not.
    (or (= \^ c r)
    ;; Only its right tile is a trap.
        (and (= \^ r) (= \. c)))))

(defn next-row [row]
  (into []
        (for [x (range (count row))]
          (let [l (get row (dec x))
                c (get row x)
                r (get row (inc x))]
            (if (is-trap l c r)
              \^
              \.)))))

(defn count-safe-in [row]
  (count (filter #(= % \.) row)))

(defn count-safe [row steps]
  (loop [row   (into [] row)
         step  0
         total 0]
    (if (= step steps)
      total
      (recur (next-row row)
             (inc step)
             (+ total (count-safe-in row))))))

(comment
  ;safe
  (is-trap \^ \^ \^)
  ;traps
  (is-trap \^ \. \.)
  (is-trap \^ \^ \.)
  (is-trap \. \^ \^)
  (is-trap \. \. \^)
  (-> (next-row (into [] "..^^."))
      next-row
      count-safe-in)
  (count-safe ".^^.^.^^^^" 10)
  ; takes ~10s
  (time (count-safe "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^......." 400000))
  )