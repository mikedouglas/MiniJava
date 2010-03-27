(ns minijava.x86.frame
  (:use clojure.contrib.str-utils
        minijava.ir)
  (:require [minijava.temp :as tm]))

(defprotocol Frame
  (allocLocal [x sym escapes?])
  (lookup [x sym])
  (obj [x])
  (fp [x])
  (rv [x])
  (formals [x])
  (word-size [x]))

(defprotocol Access
  (exp [x]))

(deftype InFrame [offset]
  Access
  (exp [] (Mem (BinaryOp :+ (Temp :bp) (Const offset)))))

(deftype InReg [temp]
  Access
  (exp [] (Temp temp)))

;; don't create directly, use (new-x86)
(deftype X86 [fp locals formals syms word]
  :as this
  Frame
  (allocLocal [sym escapes?]
    (let [loc (if escapes?
                (InFrame (swap! fp - 4))
                (InReg (tm/temp)))]
      (swap! locals conj loc)
      (swap! syms assoc sym loc)
      loc))
  (lookup [sym] (get @syms sym))
  (obj [] (first formals))
  (fp [] (deref fp))
  (rv [] (InFrame (- (:offset (obj this)) 4)))
  (formals [] formals)
  (word-size [] word))

;; (new-x86 (fp fr) 
(defn new-x86 [prev-fp args]
  (let [word    4
        fp      (atom prev-fp)
        locals  (atom [])
        formals (for [i (range (count args))]
                  (InFrame (-> i (+ 2) (* word))))
        syms    (atom (zipmap args formals))]
    (X86 fp locals formals syms word)))
