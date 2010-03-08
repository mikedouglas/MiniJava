(ns minijava.interp
  (:use (minijava ir))
  (:import (minijava.ir.temp.label)))

(defstruct env :temps :mem :labels)

(def empty-env 
  (atom (struct env (hash-map) (hash-map) (hash-map))))

(defn write-temp [env key val]
  (swap! env (fn [e k v] (assoc-in e [:temps k] v)) key val))
(defn read-temp [env key]
  (get-in @env [:temps key]))
(defn read-label [env key]
  (get-in @env [:labels key]))

(comment dispatch on type of first arg)
(defmulti eval-ir (fn [x y] (type x)))

(comment use lookahead to see if we jump or evaluate normally)
(defmethod eval-ir clojure.lang.PersistentList [lst env]
  (cond (empty? (rest lst)) ;; since dispatching on empty is annoying
          (eval-ir (first lst) env)
        (or (isa? (first lst) :minijava.ir/Jump)
            (isa? (first lst) :minijara.ir/Conditional))
          (eval-ir (first lst) env)
        true 
          (do (eval-ir (first lst) env)
              (eval-ir (rest lst) env))))

(defmethod eval-ir :minijava.ir/BinaryOp [exp env]
  (let [e1 (eval-ir (:exp1 exp) env)
        e2 (eval-ir (:exp2 exp) env)]
    (case (:op exp)
          :+ (+ e1 e2)
          :- (- e1 e2)
          :* (* e1 e2))))

(defmethod eval-ir :minijava.ir/Const [exp env]
  (:val exp))

(defmethod eval-ir :minijava.ir/Conditional [exp env]
  (let [e1 (eval-ir (:exp1 exp) env)
        e2 (eval-ir (:exp2 exp) env)]
    (case (:op exp)
          :<  (if (< e1 e2) 1 0)
          :!= (if (not (= e1 e2)) 1 0)
          :=  (if (= e1 e2) 1 0))))

; Canonicalization removes eseqs and also seqs
;
;(defmethod eval-ir ::minijava.ir/ExpSeq [exp]
;  (let [e (eval-ir (:exp exp))
;          (eval-ir (:seqs exp))]
;    (...)))
;
;(defmethod eval-ir ::minijava.ir/Seq [exp])

(defmethod eval-ir ::minijava.ir/Move [exp env]
  (let [val (eval-ir (:src exp) env)
        dst (:reg (:dst exp))]
    (write-temp env dst val)))

(defmethod eval-ir ::minijava.ir/Jump [exp env]
  (eval-ir (read-label env (:lbl exp)) env))

(comment Labels don't do anything after the label table is
         built)
(defmethod eval-ir ::minijava.ir/Label [exp env]
  nil)

(defmethod eval-ir ::minijava.ir/Mem [exp env]
  nil)

(defmethod eval-ir ::minijava.ir/Name [exp env]
  nil)

(defmethod eval-ir ::minijava.ir/Temp [exp env]
  (read-temp env (:reg exp)))

(comment Build map of label code - should be efficient by persistence
         of list data structure)
(defn build-label-map [stms map]
  (cond (empty? stms) map
        (= (type (first stms)) ::minijava.ir/Label)
          (build-label-map (rest stms)
                           (assoc map (:lbl (first stms)) (rest stms)))
        true (build-label-map (rest stms) map)))

(comment top-level eval for a sequence of statements)
(defn eval-prog [stms]
  (let [labels (build-label-map stms (hash-map))]
    (eval-ir stms 
             (atom (struct env (hash-map) (hash-map) labels)))))
