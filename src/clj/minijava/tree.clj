(ns minijava.tree
  "Converting the AST to IR as directly as possible."
  (:use (minijava ir exp label ast)))

(defmulti tree type)

(defn binop [op x]
  (BinaryOp op (unEx (tree (.e1 x))) (unEx (tree (.e2 x)))))

(defmethod tree minijava.ast.And
  [x] (let [t1   (label)
            t2   (label)
            f    (label)
            done (label)
            ret  (minijava.ir.temp.Temp.)]
        (ExpSeq [(Conditional :!= (-> x .e1 tree unEx) 0 (Name t1) (Name f))
                 (Label t1)
                 (Conditional :!= (-> x .e2 tree unEx) 0 (Name t2) (Name f))
                 (Label t2)
                 (Move (Const 1) (Temp ret))
                 (Jump done)
                 (Label f)
                 (Move (Const 0) (Temp ret))
                 (Label done)]
                (Temp ret))))

;;; NOTE: stubs have been commented out to make testing easier.

;; (defmethod tree minijava.ast.ArrayAssign
;;   [x] )

;; (defmethod tree minijava.ast.ArrayLength
;;   [x] )

;; (defmethod tree minijava.ast.Assign
;;   [x] )

(defmethod tree minijava.ast.Block
  [x] (Seq (for [s ($ (.statements x))] (tree s))))

(defmethod tree minijava.ast.BooleanLiteral
  [x] (if (.value x) (Const 1) (Const 0)))

;; (defmethod tree minijava.ast.Call
;;   [x] )

;; (defmethod tree minijava.ast.ClassDecl
;;   [x] )

;; (defmethod tree minijava.ast.IdentifierExp
;;   [x] )

(defmethod tree minijava.ast.If
  [x] (let [t (label)
            f (label)]
        (Seq [(unCx (tree (.tst x)) (Name t) (Name f))
              (Label t)
              (unNx (tree (.thn x)))
              (Label f)
              (unNx (tree (.els x)))])))

(defmethod tree minijava.ast.IntegerLiteral
  [x] (Const (.value x)))

(defmethod tree minijava.ast.LessThan
  [x] (binop :< x))

;; (defmethod tree minijava.ast.MainClass
;;   [x] )

;; (defmethod tree minijava.ast.MethodDecl
;;   [x] )

(defmethod tree minijava.ast.Minus
  [x] (binop :- x))

;; (defmethod tree minijava.ast.NewArray
;;   [x] )

;; (defmethod tree minijava.ast.Not
;;   [x] )

(defmethod tree minijava.ast.Plus
  [x] (binop :+ x))

;; (defmethod tree minijava.ast.This
;;   [x] )

(defmethod tree minijava.ast.Times
  [x] (binop :* x))

;; (defmethod tree minijava.ast.VarDecl
;;   [x] )

(defmethod tree minijava.ast.While
  [x] (let [t    (label)
            test (label)
            f    (label)]
        (Seq [(Label test)
              (unCx (tree (.tst x)) (Name t) (Name f))
              (Label t)
              (unNx (tree (.body x)))
              (Jump test)
              (Label f)])))

;; (defmethod tree minijava.sat.ArrayLookup
;;   [x] )
