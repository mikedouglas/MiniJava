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

(defn live [prog]
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

(defn conversion [program map]
  (let [all-temps (union-all (vals map))
        first-index (fn [prog map temp]
                      (loop [prog prog
                             index 0]
                        (if (contains? (get map (first prog)) temp)
                            index
                            (recur (rest prog) (inc index)))))
        last-index (fn [prog map temp] (first-index (reverse prog) map temp))]
    (map (fn [tmp]
           {:id (:id tmp),
            :start (first-index program map tmp),
            :end (last-index program map tmp)})
         all-temps)))
