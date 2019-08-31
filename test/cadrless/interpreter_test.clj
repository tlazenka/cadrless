(ns cadrless.interpreter-test
  (:require [clojure.test :refer :all]
            [cadrless.interpreter :refer :all]))

(deftest parse-test
  (testing "parse"
    (let [example (clojure.string/join "\n" [
                                             "(set i 0)"
                                             "(while (<= i 3)"
                                             "  (write i)"
                                             "  (set j 500)"
                                             "  (while (<= j 503)"
                                             "    (write j)"
                                             "    (set j (+ j 1)))"
                                             "  (set i (+ i 1)))"
                                             ])
          expected-tokens [[:open] [:symbol "set"] [:symbol "i"] [:int 0] [:close]
                           [:open] [:symbol "while"] [:open] [:symbol "<="] [:symbol "i"] [:int 3] [:close]
                           [:open] [:symbol "write"] [:symbol "i"] [:close]
                           [:open] [:symbol "set"] [:symbol "j"] [:int 500] [:close]
                           [:open] [:symbol "while"]
                           [:open] [:symbol "<="] [:symbol "j"] [:int 503] [:close]
                           [:open] [:symbol "write"] [:symbol "j"] [:close]
                           [:open] [:symbol "set"] [:symbol "j"] [:open] [:symbol "+"] [:symbol "j"] [:int 1] [:close] [:close] [:close]
                           [:open] [:symbol "set"] [:symbol "i"] [:open] [:symbol "+"] [:symbol "i"] [:int 1] [:close] [:close] [:close]]
          expected-nest {:list
                         [:symbol
                          "seq"
                          {:list
                           [{:list [{:symbol "set"} {:symbol "i"} {:int 0}]}
                            {:list
                             [{:symbol "while"}
                              {:list [{:symbol "<="} {:symbol "i"} {:int 3}]}
                              {:list [{:symbol "write"} {:symbol "i"}]}
                              {:list [{:symbol "set"} {:symbol "j"} {:int 500}]}
                              {:list
                               [{:symbol "while"}
                                {:list [{:symbol "<="} {:symbol "j"} {:int 503}]}
                                {:list [{:symbol "write"} {:symbol "j"}]}
                                {:list
                                 [{:symbol "set"}
                                  {:symbol "j"}
                                  {:list [{:symbol "+"} {:symbol "j"} {:int 1}]}]}]}
                              {:list
                               [{:symbol "set"}
                                {:symbol "i"}
                                {:list [{:symbol "+"} {:symbol "i"} {:int 1}]}]}]}]}]}
          expected-parse [:seq
                          [:set "i" {:int 0}]
                          [:while
                           [:binOp :lte {:get "i"} {:int 3}]
                           [:seq
                            [:writeInt {:get "i"}]
                            [:seq
                             [:set "j" {:int 500}]
                             [:seq
                              [:while
                               [:binOp :lte {:get "j"} {:int 503}]
                               [:seq
                                [:writeInt {:get "j"}]
                                [:set "j" [:binOp :add {:get "j"} {:int 1}]]]]
                              [:set "i" [:binOp :add {:get "i"} {:int 1}]]]]]]]
          tokenized (tokenization example)
          nested (nest tokenized)
          parsed (parse nested)
          evaluated-globals (evaluate-to-globals example)]
      (is (= tokenized expected-tokens))
      (is (= nested expected-nest))
      (is (= parsed expected-parse))
      (is (= (count evaluated-globals) 2))
      (is (= (get evaluated-globals "i") 4))
      (is (= (get evaluated-globals "j") 504)))))

(deftest reduce-test
  (testing "reduce"
    (is (= (fold-right - -8 [7 29 12 2 1]) -3))
    (is (= (reduce-right - [7 29 12 2 1]) -11))
    (is (= (fold-right vector 4 [7 29 12 2 1]) [7 [29 [12 [2 [1 4]]]]]))
    (is (= (reduce-right vector [7 29 12 2 1]) [7 [29 [12 [2 1]]]]))))
