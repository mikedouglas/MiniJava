(ns minijava.x86
  (:import (minijava.ir.frame Access Frame)
           minijava.ir.temp.Temp)
  (:use clojure.contrib.str-utils))

(deftype InFrame [offset]
  Access
  (toString [] (print-str "Frame at offset" offset))
  (exp [fp])) ;; TODO

(deftype InReg [temp]
  Access
  (toString [] (str "Register at Temp(" temp ")"))
  (exp [fp])) ;; TODO

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
                      (InReg (Temp.)))]
            (swap! locals conj loc)
            loc))
        (FP [] @fp)
        (RV [] nil)
        (getFormals [] formals)
        (wordSize [] word)
        (procEntryExit1 [body] nil)))))
