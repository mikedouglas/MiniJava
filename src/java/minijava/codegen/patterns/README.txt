This package contains a bit of infrastructure implementing IR pattern matchers.

The most important classes to look at first are:

  - IRPat: contains static methods to create various IR patterns.
  - Pat: the superclass of all patterns.
  - XXXPat: where XXX is the name of some IRNode:
        a pattern that matches such a type of node. 
        The implementation of each of these is very similar.
        Looking at a few of them should give you the general idea.
  - OrPat: a "special" pattern that matches either one of a number
           of alternate patterns.
       e.g:
         Pat<IRExp>   _x_ = Pat.any();
         Pat<Integer> _k_ = Pat.any();
         Pat<IRExp> example =
          or( BINOP( Op.PLUS, _x_, CONST(_k_)),
              BINOP( Op.PLUS, CONST(_k_), _x_) );
              
             
        