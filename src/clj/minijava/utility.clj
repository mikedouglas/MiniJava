(ns minijava.utility
  "Utility functions and macros.")

(defmacro import-ast-classes
  "Imports every class in the minijava.ast package."
  []
  (import '(minijava.ast
             And
             ArrayAssign
             ArrayLength
             ArrayLookup
             Assign
             AST
             Block
             BooleanLiteral
             BooleanType
             Call
             ClassDecl
             Expression
             IdentifierExp
             If
             IntArrayType
             IntegerLiteral
             IntegerType
             LessThan
             MainClass
             MethodDecl
             Minus
             NewArray
             NewObject
             NodeList
             Not
             ObjectType
             Plus
             Print
             Program
             Statement
             This
             Times
             Type
             VarDecl
             VarDecl$Kind
             While)))

;; (let [values 1, params 2, types 3]
;;   (named-map values params types))
;; => {:values 1, :params 2, :types 3}
(defmacro named-map
  "Returns a map of the symbols as keywords to their values."
  [& keys]
  `(zipmap (map keyword '~keys) (list ~@keys)))