(ns minijava.interp
  (:use (minijava ir))
  (:import (minijava.ir.temp.label)))

(defstruct env :temps :mem)

(def empty-env 
  (atom (struct env (hash-map) (hash-map))))

(defn write-temp [env key val]
  (assoc (:temps env) key val))
(defn read-temp [env key]
  (get (:temps env) key))

;; dispatch on type of first arg
(defmulti eval-ir (fn [x y] (type x)))

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
  (let [val (eval-ir (::src exp))
        dst (::dst exp)]
    (swap! env write-temp dst val)))

;; TODO
(defmethod eval-ir ::minijava.ir/Jump [exp env]
  (:lbl exp))

(defmethod eval-ir ::minijava.ir/Label [exp env]
  (:lbl exp))

(defmethod eval-ir ::minijava.ir/Mem [exp env]
  nil)

(defmethod eval-ir ::minijava.ir/Name [exp env]
  nil)

(defmethod eval-ir ::minijava.ir/Temp [exp env]
  (read-temp @env (::reg exp)))

;; top-level eval for a sequence of statements
(defn eval-prog [lst env]
  (if (empty? lst)
      (read-temp @env "a") ;; temporary hack
      (do (eval-ir (first lst) env)
          (eval-prog (rest lst) env))))

;(defmethod eval-jump ::minijava.ir/Jump [jmp stms env]
;  (cond (empty? stms) nil
;        (.equals (:lbl jmp) (first stms))
;        (rest stms)
;(defmethod eval-jump :default [exp stms env])
