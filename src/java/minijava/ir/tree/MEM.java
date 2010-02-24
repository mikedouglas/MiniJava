package minijava.ir.tree;

//import minijava.ir.interp.Ptr;
//import minijava.ir.interp.Word;
//import minijava.ir.interp.X86SimFrame;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class MEM extends IRExp {
	public final IRExp exp;
	public MEM(IRExp e) {exp=e;}
	@Override
	public void dump(IndentingWriter out) {
		out.print("MEM(");
		out.print(exp);
		out.print(")");
	}
	@Override
	public IRExp build(List<IRExp> kids) {
		return new MEM(kids.head());
	}
	@Override
	public List<IRExp> kids() {
		return List.list(exp);
	}
//	@Override
//	public Word interp(X86SimFrame env) {
//		//Subtle point: we only get here if the MEM is being read.
//		//The interp for MOVE treats the MEM case in its dst explicitly.
//		Ptr p = (Ptr) exp.interp(env);
//		return p.get();
//	}
//	
//	@Override
//	public void set(Word value, X86SimFrame env) {
//		Ptr d = (Ptr) exp.interp(env);
//		d.set(value);
//	}
}

