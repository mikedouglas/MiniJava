This README file is intended for comments the TA should read before 
marking your project.

At the very least YOU MUST INCLUDE THE NAMES OF EACH MEMBER OF YOUR TEAM.

Team member 1: Mike Douglas
Team member 2: Asumu Takikawa
Team member 3: Sam Bayless

If you changed any code outside of minijava.typechecker.implementation, please include 
a *brief* explanation here what you changed and why:

  We didn't modify any of the default classes, but we did add a few classes in
  minijava.visitor that implement the typechecking/classtable functionality.
  
Additional comments to the TA (if any): 

  We used a reflection-based modification to the Visitor class that is
  inspired by the Walkabout pattern from this paper by Palsberg and Jay:
    http://www.cs.ucla.edu/~palsberg/paper/compsac98.pdf
  Our Walkabout pattern also supports multiple arguments to the Visitor in
  order to pass environment data around between visits.
