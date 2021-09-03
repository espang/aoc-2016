(ns day23)

(def register
  {\a 0
   \b 0
   \c 0
   \d 0})

(defn register-loc [s]
  (when (= 1 (count s))
    (#{\a \b \c \d} (get s 0))))

(defn to-nbr [s]
  (Integer/parseInt s))

(defn value-of [s reg]
  (if-let [loc (register-loc s)]
    (reg loc)
    (to-nbr s)))

(defmulti exec first)

(defmethod exec :copy [[_ x y {:keys [reg i] :as state}]]
  (if-let [y (register-loc y)]
    (if-let [loc (register-loc x)]
      (-> state
          (assoc-in [:reg y] (reg loc))
          (update :i inc))
      (-> state
          (assoc-in  [:reg y] (to-nbr x))
          (update :i inc)))
    ; y has to be a location of a register.
    (update state :i inc)))

(defmethod exec :inc [[_ x state]]
  (if-let [x (register-loc x)]
    (-> state
        (update-in [:reg x] inc)
        (update :i inc))
    (update state :i inc)))

(defmethod exec :dec [[_ x state]]
  (if-let [x (register-loc x)]
    (-> state
        (update-in [:reg x] dec)
        (update :i inc))
    (update state :i inc)))

(defmethod exec :jump [[_ x y {:keys [reg i] :as state}]]
  (if (zero? (value-of x reg))
    (update state :i inc)
    (assoc state :i (+ i (value-of y reg)))))

(defn toggle-at [i coll]
  (if-let [entry (get coll i)]
    (assoc coll i (let [[cmd x y] entry]
                    (case cmd
                      :inc    [:dec x]
                      :dec    [:inc x]
                      :toggle [:inc x]
                      :jump   [:copy x y]
                      :copy   [:jump x y]
                      entry)))
    coll))

(defmethod exec :toggle [[_ x {:keys [reg i instructions] :as state}]]
  (-> state
      (assoc :instructions (toggle-at (+ i (value-of x reg)) instructions))
      (update :i inc)))


(comment
  (exec [:copy "a" "b" {:reg (assoc register \a 5) :i 0 :instructions []}])
  (exec [:inc "a" {:reg register :i 0 :instructions []}])
  (exec [:dec "a" {:reg register :i 0 :instructions []}])
  (exec [:jump "a" "10" {:reg register :i 0 :instructions []}])
  (exec [:jump "a" "10" {:reg (assoc register \a 1) :i 0 :instructions []}])
  (exec [:jump "1" "10" {:reg register :i 0 :instructions []}])
  (exec [:toggle "1" {:reg register :i 0 :instructions [[:inc 1] [:inc 2] [:inc 3]]}])
  ,)

(defn parse-line [line]
  (cond
    (clojure.string/starts-with? line "inc")
    (let [[_ s] (clojure.string/split line #" ")]
      [:inc s])

    (clojure.string/starts-with? line "dec")
    (let [[_ s] (clojure.string/split line #" ")]
      [:dec s])

    (clojure.string/starts-with? line "cpy")
    (let [[_ s1 s2] (clojure.string/split line #" ")]
      [:copy s1 s2])

    (clojure.string/starts-with? line "jnz")
    (let [[_ s1 s2] (clojure.string/split line #" ")]
      [:jump s1 s2])

    (clojure.string/starts-with? line "tgl")
    (let [[_ s] (clojure.string/split line #" ")]
      [:toggle s])))

(def fns (->> "input_23.txt"
              slurp
              clojure.string/split-lines
              (remove empty?)
              (mapv parse-line)))

(defn step [{:keys [instructions i] :as state}]
  (when-let [cmd (get instructions i)]
    (exec (conj cmd state))))

(defn solve [reg fns]
  (loop [state {:reg reg :i 0 :instructions fns :counter 0}]
    (if-let [state' (step state)]
      (recur (update state' :counter inc))
      state)))

(comment
  (-> {:i 0 :reg (assoc register \a 3) :instructions fns}
      step
      step)
  (time (solve (assoc register \a 7) fns))
  ; takes to long
  ; (solve (assoc register \a 12) fns)
  )
  ;

(defprotocol Expr
  (op [this])
  (change-operation [this n]))

(deftype UnaryExpr [^:unsynchronized-mutable operation
                    value]
  Expr
  (op [this] operation)
  (change-operation [this n] (set! operation n)))

(deftype BinaryExpr [^:unsynchronized-mutable operation
                     first second]
  Expr
  (op [this] operation)
  (change-operation [this n] (set! operation n)))

(defprotocol AReg
  (set [this loc v])
  (get [this loc])
  (print [this]))

(deftype Register [^:unsynchronized-mutable a
                   ^:unsynchronized-mutable b
                   ^:unsynchronized-mutable c
                   ^:unsynchronized-mutable d]
  AReg
  (set [this loc v]
       (case loc
         \a (set! a v)
         \b (set! b v)
         \c (set! c v)
         \d (set! d v)))
  (get [this loc]
       (case loc
         \a a
         \b b
         \c c
         \d d))
  (print [this] (tap> (str "Reg(" a ", " b ", " c ", " d ")"))))

(defn value-of-2 [s reg]
  (if-let [loc (register-loc s)]
    (.get reg loc)
    (to-nbr s)))

;; register and expressions are mutable
;; the function returns the new index
;; expr will be applied
(defmulti execute (fn [expr _register _index _expressions] (.op expr)))

(defmethod execute :inc [expr register index _expressions]
  (when-let [x (register-loc (.value expr))]
    (.set register x (inc (.get register x))))
  (inc index))

(defmethod execute :dec [expr register index _expressions]
  (when-let [x (register-loc (.value expr))]
    (.set register x (dec (.get register x))))
  (inc index))

(defmethod execute :copy [expr register index _expressions]
  (when-let [y (register-loc (.second expr))]
    (if-let [x (register-loc (.first expr))]
      (.set register y (.get register x))
      (.set register y (to-nbr (.first expr)))))
  (inc index))

(defmethod execute :jump [expr register index _]
  (let [val (value-of-2 (.first expr) register)]
    (if (zero? val)
      (inc index)
      (+ index (value-of-2 (.second expr) register)))))

(defmulti toggle-expr class)

(defmethod toggle-expr UnaryExpr [expr]
  (case (.op expr)
    :inc    (.change-operation expr :dec)
    :dec    (.change-operation expr :inc)
    :toggle (.change-operation expr :inc)))

(defmethod toggle-expr BinaryExpr [expr]
  (case (.op expr)
    :jump (.change-operation expr :copy)
    :copy (.change-operation expr :jump)))

(defmethod execute :toggle [expr register index expressions]
  (when-let [change-idx (+ index (value-of-2 (.value expr) register))]
    (when (<= 0 change-idx (dec (count expressions)))
      (toggle-expr (aget expressions change-idx))))
  (inc index))

(defn instruction->expression [[cmd x y]]
  (case cmd
    :inc    (->UnaryExpr :inc x)
    :dec    (->UnaryExpr :dec x)
    :toggle (->UnaryExpr :toggle x)
    :copy   (->BinaryExpr :copy x y)
    :jump   (->BinaryExpr :jump x y)))

(defn instructions->expressions [instructions]
  (let [arr (object-array (count instructions))]
    (loop [i    0
           coll (seq instructions)]
      (when (seq coll)
        (aset arr i (instruction->expression (first coll)))
        (recur (inc i) (rest coll))))
    arr))

(defn step-2 [register index expressions]
  (when (<= 0 index (dec (count expressions)))
    (execute (aget expressions index) register index expressions)))

(defn solve-2 [register expressions]
  (loop [index 0]
    (when-let [next-index (step-2 register index expressions)]
      (recur next-index))))

(defn solve-a [a expressions]
  (let [r (->Register a 0 0 0)]
    (solve-2 r expressions)
    (.get r \a)))

(comment
  (def myInc (->UnaryExpr :inc "a"))
  (def r (->Register 0 0 0 0))
  (execute myInc r 0 [])
  (.get r \a)
  ;
  (toggle-expr myInc)

  (def r2 (->Register 7 0 0 0))
  (def exprs (instructions->expressions fns))
  (solve-2 r2 exprs)
  ;
  (time (solve-a 10 (instructions->expressions fns)))
  )
