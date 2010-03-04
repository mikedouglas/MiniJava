(ns minijava.tree
  "Converting the AST to IR as directly as possible."
  (:use (minijava ir exp)))

(defmulti tree type)

(defmethod tree minijava.ast.IntegerLiteral
  [x] (Const (.value x)))

(defn binop [op x]
  (BinaryOp op (unEx (tree (.e1 x))) (unEx (tree (.e2 x)))))

(defmethod tree minijava.ast.Plus
  [x] (binop :+ x))

(defmethod tree minijava.ast.Times
  [x] (binop :* x))

(defmethod tree minijava.ast.And
  [x] (binop :& x))

(defmethod tree minijava.ast.Block
  [x] (Seq (for [s (.statements x)] (tree s))))

(defmethod tree minijava.ast.If
  [x] (let [t (Label (gensym))
            f (Label (gensym))]
        (Seq [(unCx (.tst x) (Name t) (Name f))
              t
              (unNx (tree (.then x)))
              f
              (unNx (tree (.then x)))])))

