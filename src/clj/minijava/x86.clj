(ns minijava.x86
  (:import (minijava.ir.frame))
  (:use clojure.contrib.str-utils
        minijava.ir))

(defprotocol Frame
  (allocLocal [this escapes?])
  (obj [this])
  (fp [this])
  (rv [this])
  (formals [this])
  (word-size [this]))

(defprotocol Access
  (exp [this fp]))

(deftype InFrame [offset]
  clojure.lang.IPersistentMap
  Access
  (exp [fp] (Mem (BinaryOp :+ (Temp fp) (Const offset)))))

(deftype InReg [temp]
  clojure.lang.IPersistentMap
  Access
  (exp [fp] (Temp temp)))

;; don't create directly, use (new-x86)
(deftype X86 [fp locals formals word]
  :as this
  Frame
  (allocLocal [escapes?]
    (let [loc (if escapes?
                (InFrame (swap! (:fp this) - 4))
                (InReg (minijava.ir.temp.Temp.)))]
      (swap! (:locals this) conj loc)
      loc))
  (obj [] (first (:formals this)))
  (fp [] (deref (:fp this)))
  (rv [] (merge-with + (obj this) [:offset 4]))
  (formals [] (:formals this))
  (word-size [] (:word this)))

(defn new-x86 [prev-fp num-args]
  (let [word    4
        fp      (atom prev-fp)
        locals  (atom [])
        formals (for [i (range num-args)]
                  (InFrame (-> i (+ 2) (* word))))]
    (X86 fp locals formals word)))
