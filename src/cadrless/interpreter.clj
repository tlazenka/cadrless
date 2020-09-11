(ns cadrless.interpreter)

(defn is-symbol-character [ch] (or (Character/isLetter ch) (contains? #{\+ \- \* \/ \! \= \< \> \_ \%} ch)))

(defn process-character [ch acc state]
  (let [{:keys [action type]} state]
    (case action
      :tokenizing
      (do
        (case type
          :symbol
          (if (is-symbol-character ch) [(conj acc ch) {:action :tokenizing :type :symbol}]
              [[:symbol (apply str acc)] {:action :looking}])
          :int
          (if (Character/isDigit ch) [(conj acc ch) {:action :tokenizing :type :int}]
              [[:int (Integer/parseInt (apply str acc))] {:action :looking}])

          :list
          (case acc
            [\(]
            [[:open] {:action :looking}]
            [\)]
            [[:close] {:action :looking}])))

      :looking
      (cond
        (is-symbol-character ch)
        [(conj acc ch) {:action :tokenizing :type :symbol}]

        (Character/isDigit ch)
        [(conj acc ch) {:action :tokenizing :type :int}]

        (Character/isWhitespace ch)
        [[] {:action :looking}]

        (contains? #{\( \)} ch)
        [(conj acc ch) {:action :tokenizing :type :list}]))))

(defn tokenize []
  (fn [rf]
    (let [state (volatile! {:token-acc [] :processing-state {:action :looking}})]
      (fn
        ([] (rf))
        ([result]
         (let [{:keys [token-acc processing-state]} @state
               [token-acc' _] (process-character \return token-acc processing-state)]
           (rf result token-acc')))
        ([result ch]
         (let [{:keys [token-acc processing-state]} @state
               [token-acc' processing-state'] (process-character ch token-acc processing-state)]
           (vswap! state assoc :processing-state processing-state')
           (if (keyword? (first token-acc'))
             (do
               (vswap! state assoc :token-acc [])
               (recur (rf result token-acc') ch))
             (do
               (vswap! state assoc :token-acc token-acc')
               result))))))))

(defn tokenization [s]
  (transduce (comp (tokenize) (remove empty?)) conj (seq s)))

(defn nestOne [tokens ns]
  (if (empty? tokens)
    [{:list ns}, []]
    (let [[t & ts] tokens
          key (first t)
          value (second t)]
      (case key
        :open
        (let [[n, ts'] (nestOne ts [])]
          (nestOne ts', (conj ns n)))
        :symbol
        (nestOne ts, (conj ns {:symbol value}))
        :int
        (nestOne ts, (conj ns {:int value}))
        :close
        [{:list ns}, ts]))))

(defn nest [tokens]
  (let [[result & remainder] (nestOne tokens [])]
    {:list (conj [:symbol "seq"] result)}))

(defn fold-right [f val coll]
  (if (empty? coll)
    val
    (f (first coll) (fold-right f val (rest coll)))))

(defn reduce-right [f coll]
  (fold-right f (last coll) (drop-last coll)))

(defn parse [nest]
  (cond
    (contains? nest :int)
    nest
    (contains? nest :symbol)
    {:get (:symbol nest)}
    (contains? nest :list)
    (let [{:keys [list]} nest
          [n & rest] list]
      (cond
        (= n :symbol)
        (let [[sym & rest'] rest]
          (if (= sym "seq")
            (let [{:keys [list]} (first rest')]
              (reduce-right #(vector :seq %1 %2) (map parse list)))))
        (contains? n :symbol)
        (cond
          (= (get n :symbol) "write")
          [:writeInt (parse (first rest))]
          (= (get n :symbol) "set")
          [:set (:symbol (first rest)) (parse (second rest))]
          (= (get n :symbol) "+")
          [:binOp :add (parse (first rest)) (parse (second rest))]
          (= (get n :symbol) "*")
          [:binOp :mul (parse (first rest)) (parse (second rest))]
          (= (get n :symbol) "<=")
          [:binOp :lte (parse (first rest)) (parse (second rest))]
          (= (get n :symbol) "while")
          (let [[a & bs] rest]
            [:while (parse a) (reduce-right #(vector :seq %1 %2) (map parse bs))]))))))

(defn evalOp [op, a, b]
  (case op
    :mul (* a b)
    :lte (if (<= a b) 1 0)
    :add (+ a b)))

(defn evaluation
  ([expression]
   (evaluation {} expression))
  ([vars expression]
   (cond
     (and (map? expression) (contains? expression :int))
     [(get expression :int) vars]
     (and (map? expression) (contains? expression :get))
     [(get vars (:get expression)) vars]
     (and
      (coll? expression)
      (= (first expression) :seq))
     (let [e1 (second expression)
           e2 (second (rest expression))
           [_ vars'] (evaluation vars e1)]
       (evaluation vars' e2))
     (and
      (coll? expression)
      (= (first expression) :binOp))
     (let [[_ op e1 e2] expression
           [val1, vars'] (evaluation vars e1)
           [val2, vars''] (evaluation vars' e2)]
       [(evalOp op val1 val2) vars''])
     (and
      (coll? expression)
      (= (first expression) :while))
     (let [[_ c e] expression
           [cond vars'] (evaluation vars c)]
       (if (= cond 0)
         [0, vars']
         (let [[_ vars''] (evaluation vars' e)]
           (evaluation vars'' [:while c e]))))

     (and
      (coll? expression)
      (= (first expression) :set))
     (let [variable (second expression)
           e (second (rest expression))
           [value, vars'] (evaluation vars e)]
       [value (assoc vars' variable value)])
     (and
      (coll? expression)
      (= (first expression) :writeInt))
     (let [intE (first (rest expression))
           [i vars'] (evaluation vars intE)]
       (println i)
       [i vars']))))

(defn evaluate [s]
  (let [tokenized (tokenization s)
        nested (nest tokenized)
        parsed (parse nested)]
    (evaluation {} parsed)))

(defn evaluate-to-globals [s]
  (second (evaluate s)))

(defn evaluate-to-result [s]
  (first (evaluate s)))
