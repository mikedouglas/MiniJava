(ns minijava.access
  "Datatypes for accessing registers and memory addresses."
  (:use minijava.ir))

(defprotocol Access
  (exp [x]))

(defprotocol Lookupable
  (lookup [x sym]))

(deftype InFrame [offset]
  Access
  (exp [] (Mem (BinaryOp :+ (Temp :bp) (Const offset)))))

(deftype InReg [temp]
  Access
  (exp [] (Temp temp)))

;; location of object whose method is currently being called.
(def obj-loc (InFrame 8))

(deftype InField [offset]
  Access
  (exp [] (Mem (BinaryOp :+ (exp obj-loc) (Const offset)))))
