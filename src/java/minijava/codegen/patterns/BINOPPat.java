package minijava.codegen.patterns;

import minijava.ir.tree.BINOP;
import minijava.ir.tree.IRExp;
import minijava.ir.tree.BINOP.Op;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class BINOPPat extends Pat<IRExp> {

	private Op op;
	private Pat<IRExp> l;
	private Pat<IRExp> r;

	public BINOPPat(Op op, Pat<IRExp> l, Pat<IRExp> r) {
		this.op = op;
		this.l = l;
		this.r = r;
	}

	@Override
	public void dump(IndentingWriter out) {
		out.print(op);
		out.print("(");
		out.print(l);
		out.print(",");
		out.print(r);
		out.print(")");
	}

	@Override
	public void match(IRExp toMatch, Matched matched)
			throws minijava.codegen.patterns.Pat.Failed, ClassCastException {
		BINOP binop = (BINOP) toMatch;
		if (op!=binop.getOp()) fail();
		l.match(binop.getLeft(), matched);
		r.match(binop.getRight(), matched);
		//matched.put(this, toMatch);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Pat<IRExp> build(List<Pat<?>> children) {
		return new BINOPPat(op, 
				(Pat<IRExp>)children.get(0), 
				(Pat<IRExp>)children.get(1) );
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Pat<?>> children() {
		return List.list((Pat<?>)l, r);
	}
	
}
