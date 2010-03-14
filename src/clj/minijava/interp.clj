(ns minijava.interp
  (:use minijava.ir
        clojure.contrib.pprint)
  (:import (minijava.ir.temp.label)))
 
(defstruct env :temps :mem :labels :methods)
 
(def empty-env
  (atom (struct env {} {} {} {})))
 
(defn write-temp [env key val]
  (swap! env (fn [e k v] (assoc-in e [:temps k] v)) key val))

(defn read-temp [env key]
  (get-in @env [:temps key]))

(defn read-label [env key]
  (get-in @env [:labels key]))

(defn write-mem [env addr val]
  (swap! env (fn [e a v] (assoc-in e [:mem a] v)) addr val))

(defn read-mem [env addr]
  (get-in @env [:mem addr]))

(defn read-method [env name]
  (get-in @env [:methods name]))
 
;; eval-ir evaluates the linearized IR tree. Only types that
;; appear in the final tree have cases in the multimethod.
;; e.g. ESeq and Seq are gone after linearization
 
;; dispatch on type of first arg
(defmulti eval-ir (fn [x y] (type x)))
 
;; this gets called if the program jumps to the very end
(defmethod eval-ir clojure.lang.PersistentList$EmptyList [lst env]
  nil)
 
;; use lookahead to see if we jump or evaluate normally
(defmethod eval-ir clojure.lang.ISeq [lst env]
  (cond (or (= (type (first lst)) :minijava.ir/Jump)
            (= (type (first lst)) :minijava.ir/Conditional))
          (eval-ir (first lst) env)
        (empty? (rest lst))
          (eval-ir (first lst) env)
        :else
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
        e2 (eval-ir (:exp2 exp) env)
        lt (read-label env (:lbl (:t exp)))
        lf (read-label env (:lbl (:f exp)))]
    (case (:op exp)
          :< (if (< e1 e2)
                (eval-ir lt env)
                (eval-ir lf env))
          :!= (if (not (= e1 e2))
                (eval-ir lt env)
                (eval-ir lf env))
          := (if (= e1 e2)
                (eval-ir lt env)
                (eval-ir lf env)))))
 
; Canonicalization removes eseqs and also seqs
;
;(defmethod eval-ir ::minijava.ir/ExpSeq [exp]
; (let [e (eval-ir (:exp exp))
; (eval-ir (:seqs exp))]
; (...)))
;
;(defmethod eval-ir ::minijava.ir/Seq [exp])
 
(defmethod eval-ir :minijava.ir/Move [exp env]
  (let [val (eval-ir (:src exp) env)
        dst (:dst exp)]
    (cond (= (type dst) :minijava.ir/Temp)
          (write-temp env (:reg dst) val)
          (= (type dst) :minijava.ir/Mem)
          (write-mem env (:adr dst) val))))
 
(defmethod eval-ir :minijava.ir/Jump [exp env]
  (eval-ir (read-label env (:lbl exp)) env))
 
;; Labels and names don't do anything after the label
;; table is built. These should never be called since
;; eval-ir unpacks these directly.
(defmethod eval-ir :minijava.ir/Label [exp env]
  nil)
(defmethod eval-ir :minijava.ir/Name [exp env]
  nil)
 
;; Mem does need to eval in case an operation is hiding
;; in the address expression
(defmethod eval-ir :minijava.ir/Mem [exp env]
  (let [addr (eval-ir (:adr exp) env)]
    (read-mem env addr)))
 
;; This case needed for Mem case above
(defmethod eval-ir java.lang.Integer [exp env]
  exp)
 
(defmethod eval-ir :minijava.ir/Temp [exp env]
  (read-temp env (:reg exp)))
 
;; Function call
(defmethod eval-ir :minijava.ir/Call [exp env]
  (let [fn-name (:lbl (:lbl exp))
        args (map (fn [arg] (eval-ir arg env)) (:args exp))]
    (case fn-name
      "print" (println (first args))
      "newObject" (println "Unimplemented stub")
      "newArray" []
      (eval-ir (read-method env fn-name) env))))
 
(defmethod eval-ir :minijava.ir/Statement [exp env]
  (do
    (eval-ir (:exp exp) env)
    nil)) ;; ensure no value returned
 
;; Build map of label code - should be efficient by persistence
;; of list data structure
(defn build-label-map [stms map]
  (cond (empty? stms) map
        (= (type (first stms)) :minijava.ir/Label)
          (recur (rest stms)
                 (assoc map (:lbl (first stms)) (rest stms)))
        true (recur (rest stms) map)))
 
;; top-level eval for a sequence of statements
(defn eval-prog
  ([stms] (eval-prog stms (hash-map)))
  ([stms methods]
    (let [labels (build-label-map stms (hash-map))]
      (eval-ir stms
               (atom (struct env (hash-map) (hash-map) labels))))))
