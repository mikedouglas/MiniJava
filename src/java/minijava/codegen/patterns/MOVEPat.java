package minijava.codegen.patterns;

import minijava.ir.tree.IRExp;
import minijava.ir.tree.IRStm;
import minijava.ir.tree.MOVE;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class MOVEPat extends Pat<IRStm> {

	private Pat<IRExp> dst;
	private Pat<IRExp> src;

	public MOVEPat(Pat<IRExp> dst, Pat<IRExp> src) {
		this.dst =dst;
		this.src = src;
	}

	@Override
	public void match(IRStm toMatch, Matched m)
			throws minijava.codegen.patterns.Pat.Failed, ClassCastException {
		MOVE move = (MOVE) toMatch;
		dst.match(move.dst, m);
		src.match(move.src, m);
		//m.put(this, toMatch);
	}

	@Override
	public void dump(IndentingWriter out) {
		out.print("MOVE(");
		out.print(dst);
		out.print(" <- ");
		out.print(src);
		out.print(")");
	}

	@SuppressWarnings("unchecked")
	@Override
	public Pat<IRStm> build(List<Pat<?>> children) {
		return new MOVEPat( (Pat<IRExp>)children.get(0), (Pat<IRExp>)children.get(1) );
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Pat<?>> children() {
		return List.list((Pat<?>)dst, (Pat<?>)src);
	}

}
