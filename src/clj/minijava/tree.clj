(ns minijava.tree
  "Functions to convert the AST to IR as directly as possible."
  (:use minijava.x86.frame
        (minijava access ast exp ir obj))
  (:require [minijava.temp :as tm]))

(defprotocol Treeable
  (tree [this frame]))

(def *methods* (atom (hash-map)))

(defn addMethod [name method frame]
		(reset! *methods* (assoc @*methods* name {:ir method :frame frame}))
)

(defn binop
  "Quick BinaryOp for objects with e1 and e2 fields."
  [op x frame]
  (BinaryOp op (-> x .e1 (tree frame) unEx)
            (-> x .e2 (tree frame) unEx)))

(defn apply-tree [prog]
	(tree prog nil)
		@*methods*	
)

(defn tree-prog
  "Walks through a program, applying tree."
  [seq]
  (let [frame (new-x86 0 [] nil)]
    (for [s seq] (tree s frame)))
)

(defmacro deftree [type [obj frame] & body]
  `(extend-class ~type
     Treeable
     (~'tree [~obj ~frame]
       ~@body)))

(deftree minijava.ast.And
  [x frame]
  (let [t1   (tm/label)
        t2   (tm/label)
        f    (tm/label)
        done (tm/label)
        ret  (tm/temp)]
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

(deftree minijava.ast.ArrayAssign
  [x frame]
  (Move (-> x .value (tree frame) unEx)
        (Mem (BinaryOp :+ (exp (lookup frame (.name x)))
                       (BinaryOp :* (Const (word-size frame))
                                 (-> x .index (tree frame) unEx))))))

(deftree minijava.ast.ArrayLength
  [x frame]
  (Mem (BinaryOp :- (tree (.array x) frame) (Const 1))))

(deftree minijava.ast.ArrayLookup
  [x frame]
  (Mem (BinaryOp :+ (unEx (tree (.array x) frame))
                 (BinaryOp :* (Const (word-size frame))
                           (-> x .index (tree frame) unEx)))))

(deftree minijava.ast.Assign
  [x frame]
  (Move (-> x .value (tree frame) unEx) (exp (lookup frame (.name x)))))

(deftree minijava.ast.Block
  [x frame]
  (Seq (map #(tree % frame) ($ (.statements x)))))

(deftree minijava.ast.BooleanLiteral
  [x frame]
  (if (.value x) (Const 1) (Const 0)))

(deftree minijava.ast.Call
  [x frame]
  (Call (Name (Label (.name x)))
        (map (comp unEx #(tree % frame))
             (cons (.receiver x) ($ (.rands x))))))

(deftree minijava.ast.ClassDecl
  [x frame]
 {(.name x)
   (let [obj (new-obj (for [v ($ (.vars x))] (.name v)))]
     (into {} (for [i ($ (.methods x))]
          (let [args (map #(. % name) (-> i .formals $ reverse))
						frame (new-x86 0 (cons "obj" args) obj)
						name (str (.name x) "_" (.name i))
					ir (Seq (cons (Label name) (tree i frame)))]
(addMethod name ir frame)
))))})





(deftree minijava.ast.IdentifierExp
  [x frame]
  (exp (lookup frame (.name x))))

(deftree minijava.ast.If
  [x frame]
  (let [t (tm/label)
        f (tm/label)
        d (tm/label)]
    (Seq [(-> x .tst (tree frame) (unCx (Name t) (Name f)))
          (Label t)
          (-> x .thn (tree frame) unNx)
          (Jump d)
          (Label f)
          (-> x .els (tree frame) unNx)
          (Label d)])))

(deftree minijava.ast.IntegerLiteral
  [x frame]
  (Const (.value x)))

(deftree minijava.ast.LessThan
  [x frame]
  (binop :< x frame))

(deftree minijava.ast.MainClass
  [x frame]
 (let [f (new-x86 0 ["obj"] nil)]
(addMethod "main" (tree (.statement x) f) f )))

(deftree minijava.ast.MethodDecl
  [x frame]
  (doseq [v (-> x .vars $)] (allocLocal frame (.name v) false))
  (map #(tree % frame) (-> x .statements $)))

(deftree minijava.ast.Minus
  [x frame]
  (binop :- x frame))

(deftree minijava.ast.NewArray
  [x frame]
  (Call (Name (Label "newArray")) [(-> x .size (tree frame) unEx)]))

(deftree minijava.ast.NewObject
  [x frame]
  (Call (Name (Label "newObject")) [(.typeName x)]))

(deftree minijava.ast.Not
  [x frame]
  (let [t (tm/label)
        f (tm/label)
        r (tm/temp)]
    (ExpSeq [(Move (Const 0) (Temp r))
             (-> x .e (tree frame) (unCx (Name t) (Name f)))
             (Label f)
             (Move (Const 1) (Temp r))
             (Label t)]
            (Temp r))))

(deftree minijava.ast.Plus
  [x frame]
  (binop :+ x frame))

(deftree minijava.ast.Print
  [x frame]
  (Call (Name (Label "print")) [(-> x .exp (tree frame) unEx)]))

(deftree minijava.ast.Program
  [x frame]
  (apply merge (map #(tree % nil) (cons (.mainClass x) ($ (.classes x))))))

(deftree minijava.ast.This
  [x frame]
  (exp (obj frame)))

(deftree minijava.ast.Times
  [x frame]
  (binop :* x frame))

(deftree minijava.ast.VarDecl
  [x frame]
  (assert (= (.kind x) minijava.ast.VarDecl$Kind/LOCAL))
  (allocLocal frame (.name x) false)
  (NoOp))

(deftree minijava.ast.While
  [x frame]
  (let [t    (tm/label)
        test (tm/label)
        f    (tm/label)]
    (Seq [(Label test)
          (-> x .tst (tree frame) (unCx (Name t) (Name f)))
          (Label t)
          (-> x .body (tree frame) unNx)
          (Jump test)
          (Label f)])))
