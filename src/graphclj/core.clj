(ns graphclj.core
  (:require [graphclj.graph :as graph]
            [graphclj.tools :as tools]
            [graphclj.centrality :as central])
  (:use graphclj.graph)
  (:use graphclj.centrality)
  (:use graphclj.tools)
  (:gen-class))


(defn -main
  [& args]
  (if (= (count args) 3)
    (if (= (keyword (last args)) :close)
      (print (to-dot (rank-nodes (closeness-all (erdos-renyi-rnd (Integer/parseInt (first args)) (Float/parseFloat (second args)))) (keyword (last args)))) "\n")
      (print (to-dot (rank-nodes (degrees (erdos-renyi-rnd (Integer/parseInt (first args)) (Float/parseFloat (second args)))) (keyword (last args)))) "\n"))
    (if (= (last args) :close)
      (print (to-dot (rank-nodes (closeness-all (gen-graph (readfile (first args)))) (keyword (last args)))) "\n")
      (print (to-dot (rank-nodes (degrees (gen-graph (readfile (first args)))) (keyword (last args)))) "\n"))))

;(-main 5 0.2 :close)
