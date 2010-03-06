(ns minijava.tree
  "Converting the AST to IR as directly as possible."
  (:use (minijava ir exp label ast)))

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

 ;;assign a value to a subscript of an array
 (defmethod tree minijava.ast.ArrayAssign
   [x frame] 
   (Move (tree (.value x) frame) (Mem (BinaryOp :+ (Temp (.name x)) (BinaryOp :* (4 (.index x))) ) )) ;;this could easily be incorrect. Assuming 4 byte array values
   )

;; (defmethod tree minijava.ast.ArrayLength
;;   [x] (Mem (BinaryOp :- (Temp ) (Const 1))))

 (defmethod tree minijava.ast.Assign
   [x frame]
   (Move  ( tree (.value x) frame)   (Temp (.name x))) )

(defmethod tree minijava.ast.Block
  [x frame]
  (Seq (map #(tree % frame) ($ (.statements x)))))

(defmethod tree minijava.ast.BooleanLiteral
  [x frame]
  (if (.value x) (Const 1) (Const 0)))

(defmethod tree minijava.ast.Call
  [x frame]
  (Call (Name (.name x))
        (cons (-> x .receiver (tree frame) unEx)
              (map (comp unEx #(tree % frame)) ($ (.rargs x))))))

;; (defmethod tree minijava.ast.ClassDecl
;;   [x] )

 (defmethod tree minijava.ast.IdentifierExp
   [x frame]
   (Temp (.name x)) )

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

;; (defmethod tree minijava.ast.MainClass
;;   [x] )

;; (defmethod tree minijava.ast.MethodDecl
;;   [x] )

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
        d (label)
        r (minijava.ir.temp.Temp.)]
    (ExpSeq [(-> x .e (tree frame) (unCx (Name t) (Name f)))
             (Label t)
             (Move (Const 0) (Temp r))
             (Jump d)
             (Label f)
             (Move (Const 1) (Temp r))
             (Label d)]
            (Temp r))))

(defmethod tree minijava.ast.Plus
  [x frame]
  (binop :+ x frame))

(defmethod tree minijava.ast.Print
  [x frame]
  (Call (Name "print") [(-> x .exp (tree frame) unEx)]))

 (defmethod tree minijava.ast.This
   [x frame]
   (Temp "This") ) ;;Is this the correct behaviour? 

(defmethod tree minijava.ast.Times
  [x frame]
  (binop :* x frame))

 (defmethod tree minijava.ast.VarDecl
   [x] )

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

 (defmethod tree minijava.sat.ArrayLookup
   [x frame]
   (Mem (binop :+ (tree (.array x) frame)) (binop :* 4 (tree (.index x) frame) ) )
)