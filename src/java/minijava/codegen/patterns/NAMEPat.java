package minijava.codegen.patterns;

import minijava.ir.temp.Label;
import minijava.ir.tree.IRExp;
import minijava.ir.tree.NAME;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class NAMEPat extends Pat<IRExp> {

	private Pat<Label> l;

	public NAMEPat(Pat<Label> l) {
		this.l = l;
	}

	@Override
	public void match(IRExp toMatch, Matched matched)
	throws Failed, ClassCastException {
		NAME name = (NAME) toMatch;
		l.match(name.getLabel(), matched);
	}

	@Override
	public void dump(IndentingWriter out) {
		out.print("NAME(");
		out.print(l);
		out.print(")");
	}

	@SuppressWarnings("unchecked")
	@Override
	public Pat<IRExp> build(List<Pat<?>> children) {
		return new NAMEPat((Pat<Label>)children.get(0));
	}

	@Override
	public List<Pat<?>> children() {
		return List.list(new Pat<?>[]{l});
	}
}
