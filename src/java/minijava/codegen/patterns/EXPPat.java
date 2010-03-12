package minijava.codegen.patterns;

import minijava.ir.tree.EXP;
import minijava.ir.tree.IRExp;
import minijava.ir.tree.IRStm;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class EXPPat extends Pat<IRStm> {

	private Pat<IRExp> exp;

	public EXPPat(Pat<IRExp> exp) {
		this.exp = exp;
	}

	@Override
	public void match(IRStm toMatch, Matched m)
			throws minijava.codegen.patterns.Pat.Failed, ClassCastException {
		EXP e = (EXP) toMatch;
		exp.match(e.exp, m);
		//m.put(this, toMatch);
	}

	@Override
	public void dump(IndentingWriter out) {
		out.print("EXP(");
		out.print(exp);
		out.print(")");
	}

	@SuppressWarnings("unchecked")
	@Override
	public Pat<IRStm> build(List<Pat<?>> children) {
		return new EXPPat((Pat<IRExp>)children.get(0));
	}

	@Override
	public List<Pat<?>> children() {
		return List.list(new Pat<?>[] {exp});
	}

}
