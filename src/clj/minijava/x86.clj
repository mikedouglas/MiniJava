(ns minijava.x86
  (:use clojure.contrib.str-utils
        minijava.ir))

(defprotocol Frame
  (allocLocal [this sym escapes?])
  (lookup [this sym])
  (obj [this])
  (fp [this])
  (rv [this])
  (formals [this])
  (word-size [this]))

(defprotocol Access
  (exp [this]))

(deftype InFrame [offset]
  clojure.lang.IPersistentMap
  Access
  (exp [] (Mem (BinaryOp :+ (Temp :bp) (Const offset)))))

(deftype InReg [temp]
  clojure.lang.IPersistentMap
  Access
  (exp [] (Temp temp)))

;; don't create directly, use (new-x86)
(deftype X86 [fp locals formals syms word]
  :as this
  Frame
  (allocLocal [sym escapes?]
    (let [loc (if escapes?
                (InFrame (swap! fp - 4))
                (InReg (minijava.ir.temp.Temp.)))]
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
