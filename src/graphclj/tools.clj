(ns graphclj.tools
  (:require [clojure.string :as str])
  (:use graphclj.graph)
  (:use graphclj.centrality))

(defn readfile [f]
    "Returns a sequence from a file f"
    (with-open [rdr (clojure.java.io/reader f)]
            (doall (line-seq rdr))))


(defn rank-nodes [g,l]
  "Ranks the nodes of the graph in relation to label l in accending order"
  (def element (loop [s g, res []]
                (if (seq s)
                  (recur (rest s) (conj res ((second (first s)) l)))
                  (sort (distinct res)))))
  (def map-element (loop [s element, k 0, res {}]
                    (if (seq s)
                      (recur (rest s) (inc k) (assoc res (first s) k))
                      res)))
  (loop [s g, res g]
    (if (seq s)
      (recur (rest s) (update-in res [(first (first s))] assoc :rank (map-element ((second (first s)) l))))
      res)))


;(let
;[g {1 {:neigh #{0 4 3}},
;0 {:neigh #{1 3}},
;3 {:neigh #{0 1 2}},
;4 {:neigh #{1}},
;2 {:neigh #{3}}}]
;(rank-nodes (closeness-all g) :close))


(defn generate-colors [n]
    (let [step 10]
    (loop [colors {}, current [255.0 160.0 122.0], c 0]
      (if (= c (inc n))
        colors
        (recur (assoc colors c (map #(/ (mod (+ step %) 255) 255) current))
               (map #(mod (+ step %) 255) current) (inc c))
      ))))

;(get (generate-colors 1) 1)


(defn to-dot [g]
  "Returns a string in dot format for graph g, each node is colored in relation to its ranking"
  (def premiere (loop [l "graph g{ \n", k 0]
                  (if (< k (count g))
                    (recur (str l " " k " [style=filled color=\"" (first (second (last (generate-colors (get (get g k) :rank))))) "\n " (second (second (last (generate-colors (get (get g k) :rank))))) " " (last (second (last (generate-colors (get (get g k) :rank))))) "\"]\n") (inc k))
                    (str l))))
  (def link (loop [k 0, t (into [] (get (get g k) :neigh)), res []]
              (if (< k (count g))
                (if (seq t)
                  (if (and (not (some #(= (list k (first t)) %) res)) (not (some #(= (list (first t) k) %) res)))
                    (recur k (rest t) (conj res (list k (first t))))
                    (recur k (rest t) res))
                  (recur (inc k) (into [] (get (get g (+ k 1)) :neigh)) res))
                res)))
  (def deuxieme (loop [s link, l ""]
                  (if (seq s)
                    (recur (rest s) (str l " " (first (first s)) " -- " (second (first s)) "\n"))
                    (str l "}"))))
  (str premiere deuxieme))

;(let
;[g {1 {:neigh #{0 4 3}},
;0 {:neigh #{1 3}},
;3 {:neigh #{0 1 2}},
;4 {:neigh #{1}},
;2 {:neigh #{3}}}]
;(println (to-dot (rank-nodes (closeness-all g) :close))))
