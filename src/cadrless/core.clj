(ns cadrless.core
  (:require [cadrless.interpreter :refer :all])
  (:gen-class))

(defn -main
  [& args]
  (evaluate (slurp (first args))))

