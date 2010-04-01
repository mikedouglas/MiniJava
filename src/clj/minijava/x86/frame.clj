(ns minijava.x86.frame
  (:use clojure.contrib.str-utils
        (minijava access ir))
  (:require [minijava.temp :as tm]))

(defprotocol Frame
  (allocLocal [x sym escapes?])
  (obj [x])
  (fp [x])
  (rv [x])
  (formals [x])
  (word-size [x]))

;; don't create directly, use (new-x86)
(deftype X86 [fp locals formals syms word object]
  :as this
  Frame
  (allocLocal [sym escapes?]
    (let [loc (if escapes?
                (InFrame (swap! fp - 4))
                (InReg (tm/temp)))]
      (swap! locals conj loc)
      (swap! syms assoc sym loc)
      loc))
  (obj [] (first formals))
  (fp [] (deref fp))
  (rv [] (InFrame (- (:offset (obj this)) 4)))
  (formals [] formals)
  (word-size [] word)
  Lookupable
  (lookup [sym] (or (get @syms sym) (lookup object sym))))

;; (new-x86 fp ["obj" ...])
(defn new-x86 [prev-fp args obj]
  (let [word    4
        fp      (atom prev-fp)
        locals  (atom [])
        formals (for [i (range (count args))]
                  (InFrame (-> i (+ 2) (* word))))
        syms    (atom (zipmap args formals))]
    (X86 fp locals formals syms word obj)))
