(ns minijava.liveness
  "Liveness analysis."
  (:use (minijava gas flow))
  (:require [minijava.temp :as tm])
  (:require [clojure.set :as set]))

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
          (butlast live-out))
        (let [n           (first lines)
              in          (live-in n)
              out         (live-out n)
              new-in-set  (set/union (gen (prog n))
                                     (set/difference out (kill (prog n))))
              new-out-set (set/union (kill (prog n))
                                (apply set/union (map live-in (succs n))))
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
  "Sequence of indexes where 'elm' is found in the set."
  [elm coll]
  (for [[i e] (index coll) :when (e elm)] i))

(defn convert
  "Convert the results of `live` into intervals."
  [in]
  (let [temps (apply set/union in)]
    (for [t temps]
      (let [found (pos t in)
            reg (if (keyword? (:id t)) (:id t))]
        {:id t, :start (first found), :end (last found), :reg reg}))))
