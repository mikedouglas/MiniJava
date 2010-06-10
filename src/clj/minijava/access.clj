(ns minijava.access
  "Datatypes for accessing registers and memory addresses."
  (:use minijava.ir)
  (:import [minijava.ir Mem Temp Const BinaryOp])
  (:require [minijava.temp :as tm]))

(defprotocol Lookupable
  (lookup [x sym]))

(defprotocol Access
  (exp [x]))

(defrecord InFrame [offset]
  Access
  (exp [x] (Mem. (BinaryOp. :+ (Temp. (tm/temp :EBP)) (Const. offset)))))

(defrecord InReg [temp]
  Access
  (exp [x] (Temp. temp)))

;; location of object whose method is currently being called.
(def obj-loc (InFrame. 8))

(defrecord InField [offset]
  Access
  (exp [x] (Mem. (BinaryOp. :+ (exp obj-loc) (Const. offset)))))
