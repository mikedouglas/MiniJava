package minijava.ir.tree;

//import minijava.ir.interp.X86SimFrame;
import minijava.ir.temp.Label;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class MOVE extends IRStm {
	public final IRExp dst;
	public final IRExp src;
	public MOVE(IRExp d, IRExp s) {dst=d; src=s;}
	@Override
	public void dump(IndentingWriter out) {
		out.println("MOVE(");
		out.indent();

		out.print(dst);
		out.println(" <-");
		out.print(src);

		out.outdent();
		out.print(")");
	}

	@Override
	public IRStm build(List<IRExp> kids) {
		if (dst instanceof MEM)
			return new MOVE(new MEM(kids.get(0)), kids.get(1));
		else 
			return new MOVE(dst, kids.head());
	}

	@Override
	public List<IRExp> kids() {
		if (dst instanceof MEM)
			return List.list(((MEM)dst).exp, src);
		else 
			return List.list(src);
	}
/*  FIXME: disabled interp	
	@Override
	public Label interp(X86SimFrame env) {
		dst.set(src.interp(env), env);
		return null;
	}
*/	
}

