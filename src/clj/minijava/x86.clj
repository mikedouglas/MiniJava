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

(let [word 4]
  (defn create-x86frame
    [name formals-escape]
    (let [fp      (atom 0)
          locals  (atom [])
          formals (for [i (range (count formals-escape))]
                    (InFrame (* word (+ i 2))))] ;; 8, 12, 16, ...
      (proxy [Frame] [name formals]
        (allocLocal [escapes]
          ;; allocating nonescaped locals to registers for now
          (let [loc (if escapes
                      (InFrame (swap! fp - 4))
                      (InReg (minijava.ir.temp.Temp.)))]
            (swap! locals conj loc)
            loc))
        (FP [] @fp)
        (RV [] nil)
        (getFormals [] formals)
        (wordSize [] word)
        (procEntryExit1 [body] nil)))))
