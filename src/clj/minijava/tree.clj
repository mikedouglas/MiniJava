(ns minijava.tree
  "Converting the AST to IR as directly as possible."
  (:use (minijava ir exp label ast x86)))

(defmulti tree (fn [x frame] (type x)))

(defn binop [op x frame]
  (BinaryOp op (-> x .e1 (tree frame) unEx)
            (-> x .e2 (tree frame) unEx)))

(defmethod tree minijava.ast.And
  [x frame]
  (let [t1   (label)
        t2   (label)
        f    (label)
        done (label)
        ret  (minijava.ir.temp.Temp.)]
    (ExpSeq [(-> x .e1 (tree frame) (unCx (Name t1) (Name f)))
             (Label t1)
             (-> x .e2 (tree frame) (unCx (Name t2) (Name f)))
             (Label t2)
             (Move (Const 1) (Temp ret))
             (Jump done)
             (Label f)
             (Move (Const 0) (Temp ret))
             (Label done)]
            (Temp ret))))

;;; NOTE: stubs have been commented out to make testing easier.

(defmethod tree minijava.ast.ArrayAssign
  [x frame] (Move (-> x .value tree unEx)
            (Mem (BinaryOp :+ (exp (lookup frame (.name x)))
                           (-> x .index tree unEx)))))

(defmethod tree minijava.ast.ArrayLength
  [x frame] (Mem (BinaryOp :- (tree (.array x) frame) (Const 1))))


(defmethod tree minijava.ast.Assign
  [x frame] (Move (-> x .value tree unEx) (lookup frame (.name x))))


(defmethod tree minijava.ast.Block
  [x frame]
  (Seq (map #(tree % frame) ($ (.statements x)))))

(defmethod tree minijava.ast.BooleanLiteral
  [x frame]
  (if (.value x) (Const 1) (Const 0)))

(defmethod tree minijava.ast.Call
  [x frame]
  (Call (Name (.name x))
        (map (comp unEx #(tree % frame))
             (cons (.receiver x) ($ (.rands x))))))

;; (defmethod tree minijava.ast.ClassDecl
;;   [x frame] )

(defmethod tree minijava.ast.IdentifierExp
  [x frame] (exp (lookup frame (.name x))))

(defmethod tree minijava.ast.If
  [x frame]
  (let [t (label)
        f (label)
        d (label)]
    (Seq [(-> x .tst (tree frame) (unCx (Name t) (Name f)))
          (Label t)
          (-> x .thn (tree frame) unNx)
          (Jump d)
          (Label f)
          (-> x .els (tree frame) unNx)
          (Label d)])))

(defmethod tree minijava.ast.IntegerLiteral
  [x frame]
  (Const (.value x)))

(defmethod tree minijava.ast.LessThan
  [x frame]
  (binop :< x frame))

 (defmethod tree minijava.ast.MainClass
   [x frame] 
   (tree (.statement x) frame)
   )

;; (defmethod tree minijava.ast.MethodDecl
;;   [x frame] )

(defmethod tree minijava.ast.Minus
  [x frame]
  (binop :- x frame))

(defmethod tree minijava.ast.NewArray
  [x frame]
  (Call (Name "newArray") [(-> x .size (tree frame) unEx)]))

;; FIXME: ugly, better solution? Note: not should really have a different implementation for unCx.
(defmethod tree minijava.ast.Not
  [x frame]
  (let [t (label)
        f (label)
        r (minijava.ir.temp.Temp.)]
    (ExpSeq [(Move (Const 0) (Temp r))
             (-> x .e (tree frame) (unCx (Name t) (Name f)))
             (Label f)
             (Move (Const 1) (Temp r))
             (Label t)]
            (Temp r))))

(defmethod tree minijava.ast.Plus
  [x frame]
  (binop :+ x frame))

(defmethod tree minijava.ast.Print
  [x frame]
  (Call (Name "print") [(-> x .exp (tree frame) unEx)]))

(defmethod tree minijava.ast.This
  [x frame]
  (exp (obj frame)))


(defmethod tree minijava.ast.Times
  [x frame]
  (binop :* x frame))

(defmethod tree minijava.ast.VarDecl
  [x frame]
  (cond 
   (=  (.kind x) (.LOCAL minijava.ast.VarDecl$Kind))    
   (Statement  (allocLocal frame (.name x)  false))
   (=  (.kind x)  (.FIELD minijava.ast.VarDecl$Kind))    
   (Statement nil) ;;we need to support classes before this can be figured out.
   (=  (.kind x) (.FORMAL minijava.ast.VarDecl$Kind))   
   (Statement  nil))) ;;formals are allocated at frame creation (call) so this case does nothing (?)

(defmethod tree minijava.ast.While
  [x frame]
  (let [t    (label)
        test (label)
        f    (label)]
    (Seq [(Label test)
          (-> x .tst (tree frame) (unCx (Name t) (Name f)))
          (Label t)
          (-> x .body (tree frame) unNx)
          (Jump test)
          (Label f)])))


(defmethod tree minijava.ast.ArrayLookup
  [x frame]
  (Mem (BinaryOp :+ (exp (lookup frame (.name x)))
                 (BinaryOp :* (Const 4)
                           (-> x .index tree unEx)))))

