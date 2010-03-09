(ns minijava.canon
  "Implementation of the canonicalization algorithms for IR."
  (:use minijava.ir))

(defn remove-eseq-left
  [tree]
  (let [statements (get-in tree [:exp1 :seqs])
        expression (get-in tree [:exp1 :exp])]
    (conj (vec statements)
          (merge tree [:exp1 expression]))))

(defn remove-double-eseq
  [tree]
  (let [s1 (get tree :seqs)
        s2 (get-in tree [:exp :seqs])
        e  (get-in tree [:exp :exp])]
    (conj (vec (concat s1 s2))
          e)))

(defn remove-eseq-commute
  [tree]
  (let [s1 (get-in tree [:exp1 :seqs])
        e1 (get-in tree [:exp1 :exp])
        e2 (get tree :exp2)]
    (conj (vec s1)
          (merge tree [:exp1 e1]))))

(defn remove-eseq-no-commute
  [tree]
  (let [e1 (get tree :exp1)
        s1 (get-in tree [:exp2 :seqs])
        e2 (get-in tree [:exp2 :exp])
        t  (minijava.ir.temp.Temp.)]
    (concat [(Move e1 (Temp t))]
            s1
            [(merge tree [:exp1 (Temp t)]
                    [:exp2 e2])])))
