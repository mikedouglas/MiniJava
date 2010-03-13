package minijava.codegen.patterns;

import minijava.ir.temp.Label;
import minijava.ir.tree.CJUMP;
import minijava.ir.tree.IRExp;
import minijava.ir.tree.IRStm;
import minijava.ir.tree.CJUMP.RelOp;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class CJUMPPat extends Pat<IRStm> {

	private Pat<RelOp> op;
	private Pat<IRExp> l;
	private Pat<IRExp> r;
	private Pat<Label> t;
	private Pat<Label> f;

	public CJUMPPat(Pat<RelOp> op, Pat<IRExp> l, Pat<IRExp> r, Pat<Label> t, Pat<Label> f) {
		this.op = op; 
		this.l = l;
		this.r = r;
		this.t = t;
		this.f = f;
	}

	@Override
	public void match(IRStm toMatch, Matched matched)
			throws Failed, ClassCastException {
		CJUMP cjump = (CJUMP) toMatch;
		op.match(cjump.getOp(), matched);
		l.match(cjump.getLeft(), matched);
		r.match(cjump.getRight(), matched);
		t.match(cjump.getTrueLabel(), matched);
		f.match(cjump.getFalseLabel(), matched);
		//matched.put(this, toMatch);
	}

	@Override
	public void dump(IndentingWriter out) {
		out.print("CJUMP(");
		out.print(op);
		out.print(", ");
		out.print(l);
		out.print(", ");
		out.print(r);
		out.print(", ");
		out.print(t);
		out.print(", ");
		out.print(f);
		out.print(")");
	}

	@SuppressWarnings("unchecked")
	@Override
	public Pat<IRStm> build(List<Pat<?>> children) {
		return new CJUMPPat( 
				(Pat<RelOp>)children.get(0), 
				(Pat<IRExp>)children.get(1), 	
				(Pat<IRExp>)children.get(2), 	
				(Pat<Label>)children.get(3), 	
				(Pat<Label>)children.get(4) ); 	
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Pat<?>> children() {
		return List.list(
				(Pat<?>)op, l, r, t, f );
	}

}
