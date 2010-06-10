(ns minijava.x86.frame
  (:use [minijava access ir])
  (:import [minijava.access InFrame InReg])
  (:require [minijava.temp :as tm]))

(defprotocol Frame
  (allocLocal [x sym escapes?])
  (obj [x] "Location of object.")
  (fp [x] "Frame pointer.")
  (rv [x] "Return value.")
  (formals [x] "Arguments.")
  (word-size [x] "Size of word in CPU."))

;; don't create directly, use (new-x86)
(defrecord X86 [fp locals formals syms word object]
  Frame
  (allocLocal [x sym escapes?]
    (let [loc (if escapes?
                (InFrame. (swap! fp - 4))
                (InReg. (tm/temp)))]
      (swap! locals conj loc)
      (swap! syms assoc sym loc)
      loc))
  (obj [x] (first formals))
  (fp [x] (deref fp))
  (rv [x] (InFrame. (- (:offset (obj x)) 4)))
  (formals [x] formals)
  (word-size [x] word)
  Lookupable
  (lookup [x sym] (or (get @syms sym) (lookup object sym))))

;; (new-x86 fp ["obj" ...])
(defn new-x86 [prev-fp args obj]
  (let [word    4
        fp      (atom prev-fp)
        locals  (atom [])
        formals (for [i (range (count args))]
                  (InFrame. (-> i (+ 2) (* word))))
        syms    (atom (zipmap args formals))]
    (X86. fp locals formals syms word obj)))
