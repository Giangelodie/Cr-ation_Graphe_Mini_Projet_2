(ns graphclj.graph
  (:require [clojure.string :as str]))

;; Generate a graph from the lines
(defn split-by-whitespace [s]
	(clojure.string/split s #"\s+"))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn transform [u]
  (loop [s u, k 0, res [] ]
    (if (seq s)
      (recur (rest s) (inc k) (conj res (map #(Integer/parseInt %) (split-by-whitespace (nth u k)))))
      res)))

(defn create-map [u]
  (loop [s u, res {}]
    (if (seq s)
      (recur (rest s) (assoc res (first s) {:neigh #{}}))
      res)))

(defn gen-graph [lines]
  "Returns a hashmap contating the graph"
  (if (instance? java.lang.String (first lines))
    (def lines-trans (transform lines))
    (def lines-trans lines))
  (let [elements (distinct (flatten lines-trans))]
    (let [map-base (create-map elements)]
      (loop [k 0, s lines-trans, res map-base]
        (if (seq s)
          (if (= (mod k 2) 0)
            (if (= (count (first s)) 2)
              (if (not (= (first (first s)) nil))
                (recur (inc k) s (update-in res [(first (first s)) :neigh] conj (second (first s))))
                (recur (inc k) s res))
              (recur (inc k) s res))
            (if (= (count (first s)) 2)
              (if (not (= (second (first s)) nil))
                (recur (inc k) (rest s) (update-in res [(second (first s)) :neigh] conj (first (first s))))
                (recur (inc k) (rest s) (update-in res [(first (first s)) :neigh] conj (second (first s)))))
              (recur (inc k) (rest s) res)))
          res)))))

;(gen-graph '( "0 1" "2 3" "5" "0 2"))


(defn convert-list[n]
  (loop [k 0, res []]
    (if (< k n)
      (recur (inc k) (conj res k))
      res)))

(defn perm1 [v]
  (let [len (count v), j (loop [i (- len 2)]
       (cond (= i -1) nil
  	   (< (v i) (v (inc i))) i
  	   :else (recur (dec i))))]
    (when j
      (let [vj (v j),
	    l (loop [i (dec len)]
		(if (< vj (v i)) i (recur (dec i))))]
	(loop [v (assoc v j (v l) l vj), k (inc j), l (dec len)]
	  (if (< k l)
	    (recur (assoc v k (v l) l (v k)) (inc k) (dec l))
	    v))))))


(defn vec-perm [u]
  (when u (cons u (lazy-seq (vec-perm (perm1 u))))))

(defn permutations-beta [u]
  (lazy-seq
   (let [vec1 (vec (sort u))]
     (if (zero? (count vec1))
       (list [])
       (vec-perm vec1)))))

(defn permutations [i]
  (let [v (vec i)]
    (map #(map v %) (permutations-beta (range (count v))))))

(defn drop-combi [u]
  (loop [s u, res []]
    (if (seq s)
      (recur (rest s) (conj res (take 2 (first s))))
      res)))

(defn verif [n u]
  (def liste1 (into [] (range n)))
  (def v (into [] (sort (distinct (into [] (flatten u))))))
  (loop [s liste1, res u]
    (if (seq s)
      (if (not (some #(= (first s) %) v))
          (recur (rest s) (conj res (conj () (first s))))
          (recur (rest s) res))
      res)))



(defn erdos-renyi-rnd [n, p]
  "Returns a G_{n,p} random graph, also known as an Erdős-Rényi graph"
  (let [liste (convert-list n)]
    (let [combi (distinct (drop-combi (permutations liste)))]
      (let [combi-prob (random-sample p combi)]
        (let [combi-prob2 (verif n combi-prob)]
          (gen-graph combi-prob2))))))


;(erdos-renyi-rnd 5 0.2)
