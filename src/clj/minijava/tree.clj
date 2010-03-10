(ns minijava.tree
  "Converting the AST to IR as directly as possible."
  (:use (minijava ir exp label ast x86)))

(defprotocol Treeable
  (tree [this frame]))

(defn binop
  "Quick BinaryOp for objects with e1 and e2 fields."
  [op x frame]
  (BinaryOp op (-> x .e1 (tree frame) unEx)
            (-> x .e2 (tree frame) unEx)))

(defn tree-prog [seq]
  (let [frame (new-x86 0 [])]
    (for [s seq] (tree s frame))))

(extend-class minijava.ast.And
  Treeable
  (tree [x frame]
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
              (Temp ret)))))

(extend-class minijava.ast.ArrayAssign
  Treeable
  (tree [x frame]
    (Move (-> x .value (tree frame) unEx)
          (Mem (BinaryOp :+ (exp (lookup frame (.name x)))
                         (BinaryOp :* (Const (word-size frame))
                                   (-> x .index (tree frame) unEx)))))))

(extend-class minijava.ast.ArrayLength
  Treeable
  (tree [x frame]
    (Mem (BinaryOp :- (tree (.array x) frame) (Const 1)))))

(extend-class minijava.ast.Assign
  Treeable
  (tree [x frame]
    (Move (-> x .value (tree frame) unEx) (exp (lookup frame (.name x))))))

(extend-class minijava.ast.Block
  Treeable
  (tree [x frame]
    (Seq (map #(tree % frame) ($ (.statements x))))))

(extend-class minijava.ast.BooleanLiteral
  Treeable
  (tree [x frame]
    (if (.value x) (Const 1) (Const 0))))

(extend-class minijava.ast.Call
  Treeable
  (tree [x frame]
    (Call (Name (.name x))
          (map (comp unEx #(tree % frame))
               (cons (.receiver x) ($ (.rands x)))))))

(extend-class minijava.ast.ClassDecl
  Treeable
  (tree [x frame]
    {(.name x)
     (into {} (for [i ($ (.methods x))]
                (let [frame (new-x86 0 (map #(. % name) ($ (.vars i))))
                      name  (str (.name x) "_" (.name i))]
                  [name (Seq (cons (Label name) (tree i frame)))])))}))

(extend-class minijava.ast.IdentifierExp
  Treeable
  (tree [x frame]
    (exp (lookup frame (.name x)))))

(extend-class minijava.ast.If
  Treeable
  (tree [x frame]
    (let [t (label)
          f (label)
          d (label)]
      (Seq [(-> x .tst (tree frame) (unCx (Name t) (Name f)))
            (Label t)
            (-> x .thn (tree frame) unNx)
            (Jump d)
            (Label f)
            (-> x .els (tree frame) unNx)
            (Label d)]))))

(extend-class minijava.ast.IntegerLiteral
  Treeable
  (tree [x frame]
    (Const (.value x))))

(extend-class minijava.ast.LessThan
  Treeable
  (tree [x frame]
    (binop :< x frame)))

(extend-class minijava.ast.MainClass
  Treeable
  (tree [x frame]
    {"main" (tree (.statement x) (new-x86 0 ["obj"]))}))

(extend-class minijava.ast.MethodDecl
  Treeable
  (tree [x frame]
    (map #(tree % frame) (concat (-> x .vars $)
                                 (-> x .statements $)))))

(extend-class minijava.ast.Minus
  Treeable
  (tree [x frame]
    (binop :- x frame)))

(extend-class minijava.ast.NewArray
  Treeable
  (tree [x frame]
    (Call (Name "newArray") [(-> x .size (tree frame) unEx)])))

(extend-class minijava.ast.NewObject
  Treeable
  (tree [x frame]
    (Call (Name "newObject") [(.typeName x)])))

(extend-class minijava.ast.Not
  Treeable
  (tree [x frame]
    (let [t (label)
          f (label)
          r (minijava.ir.temp.Temp.)]
      (ExpSeq [(Move (Const 0) (Temp r))
               (-> x .e (tree frame) (unCx (Name t) (Name f)))
               (Label f)
               (Move (Const 1) (Temp r))
               (Label t)]
              (Temp r)))))

(extend-class minijava.ast.Plus
  Treeable
  (tree [x frame]
    (binop :+ x frame)))

(extend-class minijava.ast.Print
  Treeable
  (tree [x frame]
    (Call (Name "print") [(-> x .exp (tree frame) unEx)])))

(extend-class minijava.ast.Program
  Treeable
  (tree [x frame]
    (apply merge (map #(tree % nil)
                      (cons (.mainClass x) ($ (.classes x))) (repeat nil)))))

(extend-class minijava.ast.This
  Treeable
  (tree [x frame]
    (exp (obj frame))))

(extend-class minijava.ast.Times
  Treeable
  (tree [x frame]
    (binop :* x frame)))

(extend-class minijava.ast.VarDecl
  Treeable
  (tree [x frame]
    (condp = (.kind x)
      minijava.ast.VarDecl$Kind/LOCAL (do (allocLocal frame (.name x) false) (NoOp))
      minijava.ast.VarDecl$Kind/FIELD (throw (Exception. "shouldn't be called"))
      minijava.ast.VarDecl$Kind/FORMAL (throw (Exception. "shouldn't be called")))))

(extend-class minijava.ast.While
  Treeable
  (tree [x frame]
    (let [t    (label)
          test (label)
          f    (label)]
      (Seq [(Label test)
            (-> x .tst (tree frame) (unCx (Name t) (Name f)))
            (Label t)
            (-> x .body (tree frame) unNx)
            (Jump test)
            (Label f)]))))

(extend-class minijava.ast.ArrayLookup
  Treeable
  (tree [x frame]
    (Mem (BinaryOp :+ (unEx (tree (.array x) frame))
                   (BinaryOp :* (Const (word-size frame))
                             (-> x .index (tree frame) unEx))))))
