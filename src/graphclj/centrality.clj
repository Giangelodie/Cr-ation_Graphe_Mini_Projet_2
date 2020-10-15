(ns graphclj.centrality
    (:require [graphclj.graph :as graph]
              [clojure.set :as set])
    (:use graphclj.graph))


(defn degrees [g]
  (let [elements (keys g)]
    (let [map (create-map elements)]
      (loop [s g, res g]
        (if (seq s)
          (recur (rest s) (update-in res [(first (first s))] assoc :degree (count ((second (first s)) :neigh))))
          res)))))

;(let
;[g {1 {:neigh #{0 4 3}},
;0 {:neigh #{1 3}},
;3 {:neigh #{0 1 2}},
;4 {:neigh #{1}},
;2 {:neigh #{3}}}]
;(degrees g))


(defn voisin [g]
  (def val1 (map val g))
  (def vois (loop [s val1, res []]
              (if (seq s)
                (recur (rest s) (conj res (into ()(get (first s) :neigh))))
                (into #{} (distinct (flatten res))))))
  (remove (into #{} (keys g)) vois))

(defn selec [g u]
  (def u1 (into [] u))
  (println u1)
  (loop [s g, res {}]
    (if (seq s)
      (if (some #(= (first (first s)) %) u1)
        (recur (rest s) (conj res (first s)))
        (recur (rest s) res))
      res)))

(defn distance3 [g n]
  (def g2 (loop [s g, res {}]
            (if (seq s)
              (if (.contains (into [] ((second (first s)) :neigh)) n)
                (recur (rest s) (conj res (first s)))
                (recur (rest s) res))
              res)))
  (def elements (loop [s g2, res {}]
                  (if (seq s)
                    (recur (rest s) (assoc res (first (first s)) 1.0))
                    res)))
  (loop [s g2, k 2.0, t (voisin s), res elements]
    (if (not (= (count res) (count g)))
      (if (seq t)
        (if (= (first t) n)
          (recur s k (rest t) (assoc res (first t) 0.0))
          (recur s k (rest t) (assoc res (first t) k)))
        (recur (selec g (voisin s)) (inc k) (voisin (selec g (voisin s))) res))
      res)))


;(let
;[g {1 {:neigh #{0 4 3}},
;0 {:neigh #{1 3}},
;3 {:neigh #{0 1 2}},
;4 {:neigh #{1}},
;2 {:neigh #{3}}}]
;(distance3 g 0))


(defn closeness [g n]
  "Returns the closeness for node n in graph g"
  (def distance (distance3 g n))
  (loop [s distance, k 0]
    (if (seq s)
      (if (not (= (second (first s)) 0.0))
        (recur (rest s) (+ k (/ 1 (second (first s)))))
        (recur (rest s) k))
      k)))



;(let
;[g {1 {:neigh #{0 4 3}},
;0 {:neigh #{1 3}},
;3 {:neigh #{0 1 2}},
;4 {:neigh #{1}},
;2 {:neigh #{3}}}]
;(closeness g 0))

(defn closeness-all [g]
  "Returns the closeness for all nodes in graph g"
  (loop [s g, res g]
    (if (seq s)
      (recur (rest s) (update-in res [(first (first s))] assoc :close (closeness g (first (first s)))))
      res)))


;(let
;[g {1 {:neigh #{0 4 3}},
;0 {:neigh #{1 3}},
;3 {:neigh #{0 1 2}},
;4 {:neigh #{1}},
;2 {:neigh #{3}}}]
;(closeness-all g))
