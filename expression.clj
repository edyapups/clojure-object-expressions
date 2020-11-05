(def exp #(Math/exp %))
(defn divide [x y] (/ (double x) (double y)))
(defn sumexp [& args] (apply + (mapv exp args)))
(defn proto-get [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :proto) (proto-get (obj :proto) key)
    :else nil))

(defn proto-call [this key & args]
  (apply (proto-get this key) this args))

(defn field [key]
  (fn [this] (proto-get this key)))

(defn method [key]
  (fn [this & args] (apply proto-call this key args)))

(defn applier [constructor prototype]
  (fn [& args] (apply constructor {:proto prototype} args)))

(def evaluate (method :evaluate))

(def toString (method :toString))

(def diff (method :diff))

(defn expPrototype [toStr eval diff]
  {:evaluate eval
   :toString toStr
   :diff     diff})

(def opPrototype (let [_op (field :op)
                       _operator (field :operator)
                       _diffRule (field :diffRule)
                       _args (field :args)]
                   (expPrototype
                     (fn [this]
                       (str "(" (_operator this) " " (clojure.string/join " " (mapv toString (_args this))) ")")),
                     (fn [this vars] (apply (_op this) (mapv #(evaluate % vars) (_args this)))),
                     (fn [this name] ((_diffRule this) (_args this) (mapv #(diff % name) (_args this))))
                     )))

(declare ZERO)
(def Constant (let [_val (field :val)]
                (applier
                  (fn [this val] (assoc this :val val))
                  (expPrototype (fn [this] (format "%.1f" (_val this))), (fn [this vars] (_val this)),
                                (fn [this name] ZERO))
                  )))
(def ZERO (Constant 0))
(def ONE (Constant 1))

(def Variable (let [_name (field :name)]
                (applier
                  (fn [this name] (assoc this :name name))
                  (expPrototype _name, (fn [this vars] (get vars (_name this))),
                                (fn [this name] (if (= (_name this) name)
                                                  ONE
                                                  ZERO))))))

(defn Operation [op operator diffRule]
  (applier
    (fn [this & args] (assoc this :args (vec args)))
    {:proto    opPrototype
     :op       op
     :operator operator
     :diffRule diffRule}))

(def Negate (Operation -, 'negate, (fn [args d_args] (Negate (first d_args)))))

(def Add (Operation +, '+, (fn [args d_args] (apply Add d_args))))

(def Subtract (Operation -, '-, (fn [args d_args] (apply Subtract d_args))))

(declare Multiply)

(defn mul_diff [args d_args]
  (second (reduce
            (fn [[a da] [b db]] [(Multiply a b), (Add (Multiply da b), (Multiply a db))])
            (mapv vector args d_args))))

(def Multiply (Operation *, '*, mul_diff))

(declare Divide)
(defn div_diff [[x & rest_x] [dx & rest_dx]]
  (let [rest (apply Multiply rest_x), diff_rest (mul_diff rest_x rest_dx)]
    (if (empty? rest_x)
      (Negate (Divide dx (Multiply x x)))
      (Divide (Subtract (Multiply dx rest) (Multiply x diff_rest)) (Multiply rest rest)))))

(def Divide (Operation (fn ([x] (/ (double x))) ([x & rst] (reduce divide x rst))), '/, div_diff))

(declare Sumexp)
(defn diff_sumexp [args d_args]
  (apply Add
         (mapv (fn [x y] (Multiply (Sumexp x) y))
               args d_args)))
(def Sumexp (Operation sumexp, 'sumexp, diff_sumexp))

(def Softmax (Operation
               (fn [& args] (divide (exp (first args)) (apply sumexp args))),
               'softmax,
               (fn [args d_args]
                 (div_diff [(Sumexp (first args))
                            (apply Sumexp args)]
                           [(diff_sumexp (vector (first args)) (vector (first d_args)))
                            (diff_sumexp args d_args)]))))

(def operators {
                '+       Add,
                '-       Subtract,
                '*       Multiply,
                '/       Divide,
                'negate  Negate,
                'sumexp  Sumexp,
                'softmax Softmax
                })

(defn parse [token]
  (cond
    (list? token) (apply (get operators (first token)) (mapv parse (rest token)))
    (number? token) (Constant token)
    :else (Variable (str token))
    ))

(defn parseObject [expression]
  (parse (read-string expression)))