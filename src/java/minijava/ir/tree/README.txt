This package contains code corresponding to the code supplied in the textbook
package called "Tree".

Differences to the book's code:

1) package renamed: 

Here it is called minijava.irtree, which is more in keeping with Java's
naming conventions and a little more descriptive.

"irtree" stands for "Internediate Representation Tree" (as opposed abstract syntax tree)

2) renamed abstract classes.

The classes in this package that represent concrete IR nodes have the same
names as in the book. They are written in all capital letters. 
  e.g. JUMP, LABEL, ...
  
Abstract classed used as superclasses for these classes have been 
renamed to start with "IR", to more easily distinguish them from 
similar classes in other packages. For example, the class Tree.Exp from the 
book is now called IRExp to avoid confusion with similarly named classes in the
ast package and the translate package.

3) Print.java

This has been replaced by implementations of the minijava.util.Indentable
interface (a method called "dump") in each of the IRNode classes.

We do not use Temp maps in this printing implementation, but it is possible to
include a "special printing name" when a Temp is created.