package minijava.ir.tree;

//import minijava.ir.interp.X86SimFrame;
import minijava.ir.temp.Label;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class EXP extends IRStm {
	public final IRExp exp; 
	public EXP(IRExp e) {exp=e;}
	@Override
	public void dump(IndentingWriter out) {
		out.print("EXP(");
		out.print(exp);
		out.print(")");
	}
	@Override
	public IRStm build(List<IRExp> kids) {
		return IR.EXP(kids.get(0));
	}
	@Override
	public List<IRExp> kids() {
		return List.list(exp);
	}
/*  FIXME: disabled interp
	@Override
	public Label interp(X86SimFrame env) {
		exp.interp(env);
		return null;
	}
*/
}

