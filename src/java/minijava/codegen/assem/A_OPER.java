package minijava.codegen.assem;

import minijava.ir.temp.Label;
import minijava.ir.temp.Temp;
import minijava.util.List;

/**
 * Use this class to represent most instructions, 
 * except MOV temp <- temp 
 * and labels.
 */
public class A_OPER extends Instr {
   public List<Temp> dst;   
   public List<Temp> src;
   public List<Label> jump;

   public A_OPER(String a, List<Temp> d, List<Temp> s, List<Label> j) {
      super(a); 
      dst= d; 
      src= s; 
      jump= j;
   }
   
   public A_OPER(String a, List<Temp> d, List<Temp> s) {
	   this(a, d, s, null);
   }

   @Override
   public List<Temp> use() {return src;}
   @Override
   public List<Temp> def() {return dst;}
   @Override
   public List<Label> jumps() {return jump;}

}
