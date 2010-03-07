(ns minijava.x86
  (:import (minijava.ir.frame Access Frame))
  (:use clojure.contrib.str-utils
        minijava.ir))

(deftype InFrame [offset]
  Access
  (exp [fp] (Mem (BinaryOp :+ (Temp fp) (Const offset)))))

(deftype InReg [temp]
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
  (fp [] (InFrame (deref (:fp this))))
  (rv [] (InFrame (- (:offset (obj this)) 4)))
  (formals [] (:formals this))
  (word-size [] (:word this)))

(defn new-x86 [prev-fp num-args]
  (let [word    4
        fp      (atom prev-fp)
        locals  (atom [])
        formals (for [i (range num-args)]
                  (InFrame (-> i (+ 2) (* word))))]
    (X86 fp locals formals word)))
