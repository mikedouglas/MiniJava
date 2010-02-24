package minijava.ir.tree;

//import minijava.ir.interp.Callable;
//import minijava.ir.interp.Word;
//import minijava.ir.interp.X86SimFrame;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class CALL extends IRExp {
	private IRExp func;
	private List<IRExp> args;
	
	public CALL(IRExp f, List<IRExp> a) {func=f; args=a;}
	
	@Override
	public void dump(IndentingWriter out) {
		out.println("CALL(");
		out.indent();
		
		out.print(func);
		for (IRExp arg : args) {
			out.println(",");
			out.print(arg);
		}
		out.outdent();
		out.print(")");
	}

	@Override
	public IRExp build(List<IRExp> kids) {
		return new CALL(kids.head(), kids.tail());
	}

	@Override
	public List<IRExp> kids() {
		return List.cons(func, args);
	}

//	@Override
//	public Word interp(X86SimFrame env) {
//		Callable procVal = (Callable) func.interp(env);
//		List<Word> argVals = List.list();
//		for (IRExp arg : args) {
//			argVals.add(arg.interp(env));
//		}
//		return procVal.call(env.getInterp(), argVals);
//	}

	public IRExp getFunc() {
		return func;
	}

	public List<IRExp> getArgs() {
		return args;
	}
}

