(ns minijava.x86
  (:import (minijava.ir.frame Access Frame)
           minijava.ir.temp.Temp))

(defn in-frame
  "A placeholder in the frame."
  [offset]
  (proxy [Access] []
    (toString [] (pr-str "Frame at offset" offset))
    (exp [fp] nil)))

(defn in-reg
  "A register for locals."
  [temp]
  (proxy [Access] []
    (toString [] (str "Register at Temp(" temp ")"))
    (exp [fp] nil)))

(let [word 4]
  (defn create-x86frame
    [name formals-escape]
    (let [fp      (atom 0)
          locals  (atom [])
          formals (for [i (range (count formals-escape))]
                    (in-frame (* word (+ i 2))))] ;; 8, 12, 16, ...
      (proxy [Frame] [name formals]
        (allocLocal [escapes]
          ;; allocating nonescaped locals to registers for now
          (let [loc (if escapes
                      (in-frame (swap! fp - 4))
                      (in-reg (Temp.)))]
            (swap! locals conj loc)
            loc))
        (FP [] @fp)
        (RV [] nil)
        (getFormals [] formals)
        (wordSize [] word)
        (procEntryExit1 [body] nil)))))