package minijava.codegen.patterns;

import minijava.ir.tree.CALL;
import minijava.ir.tree.IRExp;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class CALLPat extends Pat<IRExp> {

	private Pat<IRExp> func;
	private Pat<List<IRExp>> args;

	public CALLPat(Pat<IRExp> func, Pat<List<IRExp>> args) {
		this.func = func;
		this.args = args;
	}

	@Override
	public void match(IRExp toMatch, Matched matched) 
			throws Failed, ClassCastException {
		CALL call = (CALL) toMatch;
		func.match(call.getFunc(), matched);
		args.match(call.getArgs(), matched);
		//matched.put(this, toMatch);
	}

	@Override
	public void dump(IndentingWriter out) {
		out.print("CALL(");
		out.print(func);
		out.print(" | ");
		out.print(args);
		out.print(")");
	}

	@SuppressWarnings("unchecked")
	@Override
	public Pat<IRExp> build(List<Pat<?>> children) {
		return new CALLPat( 
				(Pat<IRExp>)children.get(0),
				(Pat<List<IRExp>>)children.get(1));
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Pat<?>> children() {
		return List.list((Pat<?>)func, args);
	}

}
