(ns minijava.liveness
  (:use (minijava gas flow))
  (:require [minijava.temp :as tm])
  (:require [clojure.set :as set]))

;; union all the sets in the list together into one set
(defn union-all [sets-to-union]
  (loop [sets sets-to-union
         u    #{}]
   (if (empty? sets)
       u ;; return the complete union of the sets
       (recur (rest sets) (set/union u (first sets))))))

(defn live
  "Creates a vector of the live-set at each line in the program."
  [prog]
  (let [succs (vec (flow prog))]
    (loop [lines    (-> prog count range reverse)
           live-in  (vec (take (inc (count prog)) (repeat #{})))
           live-out (vec (take (inc (count prog)) (repeat #{})))
           changed  true]
      (if (empty? lines)
        (if changed
          (recur (-> prog count range reverse) live-in live-out false)
          (butlast live-in))
        (let [n           (first lines)
              in          (live-in n)
              out         (live-out n)
              new-in-set  (set/union (gen (prog n))
                                     (set/difference out (kill (prog n))))
              new-out-set (union-all (map live-in (succs n)))
              new-ins     (assoc live-in n new-in-set)
              new-outs    (assoc live-out n new-out-set)
              changed?    (or changed
                              (not (= in new-in-set))
                              (not (= out new-out-set)))]
          (recur (rest lines) new-ins new-outs changed?))))))

(defn- index
  [coll]
  (map vector (iterate inc 0) coll))

(defn- pos
  [pred coll]
  (for [[i e] (index coll) :when (pred e)] i))

(defn convert
  "Convert the results of `live` into intervals."
  [in]
  (let [temps (union-all (vals map))]
    (for [t temps]
      (let [found (pos #{t} in)]
        {:id t, :start (first found), :end (last found)}))))
