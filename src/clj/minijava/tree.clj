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
        (ExpSeq [(-> x .e1 tree (unCx (Name t1) (Name f)))
                 (Label t1)
                 (-> x .e2 tree (unCx (Name t2) (Name f)))
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
;;   [x] (Mem (BinaryOp :- (Temp ) (Const 1))))

;; (defmethod tree minijava.ast.Assign
;;   [x] )

(defmethod tree minijava.ast.Block
  [x] (Seq (for [s ($ (.statements x))] (tree s))))

(defmethod tree minijava.ast.BooleanLiteral
  [x] (if (.value x) (Const 1) (Const 0)))

(defmethod tree minijava.ast.Call
  [x] (Call (Name (label (.name x)))
            (for [a ($ (.rargs x))] (unEx (tree a)))))

;; (defmethod tree minijava.ast.ClassDecl
;;   [x] )

;; (defmethod tree minijava.ast.IdentifierExp
;;   [x] )

(defmethod tree minijava.ast.If
  [x] (let [t (label)
            f (label)
            d (label)]
        (Seq [(unCx (tree (.tst x)) (Name t) (Name f))
              (Label t)
              (unNx (tree (.thn x)))
              (Jump d)
              (Label f)
              (unNx (tree (.els x)))
              (Label d)])))

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

;; FIXME: ugly, better solution?
(defmethod tree minijava.ast.Not
  [x] (let [t (label)
            f (label)
            d (label)
            r (minijava.ir.temp.Temp.)]
        (ExpSeq [(unCx (tree (.e x)) (Name t) (Name f))
                 (Label t)
                 (Move (Const 0) (Temp r))
                 (Jump d)
                 (Label f)
                 (Move (Const 1) (Temp r))
                 (Label d)]
                (Temp r))))

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