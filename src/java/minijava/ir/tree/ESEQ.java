package minijava.ir.tree;

//import minijava.ir.interp.Word;
//import minijava.ir.interp.X86SimFrame;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class ESEQ extends IRExp {
	public final IRStm stm;
	public final IRExp exp;
	public ESEQ(IRStm s, IRExp e) {stm=s; exp=e;}
	@Override
	public void dump(IndentingWriter out) {
		out.println("ESEQ(");
		out.indent();
		
		out.print(stm);
		out.println(",");
		out.print(exp);
		
		out.outdent();
		out.print(")");
	}
	@Override
	public IRExp build(List<IRExp> kids) {
		throw new Error("Not applicable to ESEQ");
	}
	@Override
	public List<IRExp> kids() {
		throw new Error("Not applicable to ESEQ");
	}
//	@Override
//	public Word interp(X86SimFrame env) {
//		throw new Error("ESEQ is not atomic! Can only interp atomic statments!\n" +
//				        "  (linearized IR should not have any ESEQ!)");
//	}
}

