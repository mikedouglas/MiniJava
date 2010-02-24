package minijava.ir.tree;

//import minijava.ir.interp.Word;
//import minijava.ir.interp.X86SimFrame;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class BINOP extends IRExp {

	private Op binop;
	private IRExp left, right;
	
	public IRExp getLeft() {
		return left;
	}

	public IRExp getRight() {
		return right;
	}

	public enum Op {
		PLUS, MINUS, MUL, DIV, 
		AND,OR,LSHIFT,RSHIFT,ARSHIFT,XOR;
	}

	public BINOP(Op b, IRExp l, IRExp r) {
		binop=b; left=l; right=r; 
	}

	@Override
	public void dump(IndentingWriter out) {
		out.print("BINOP(");
		out.print(binop);
		out.println(",");
		out.indent();
		
		out.print(left);
		out.println(", ");
		out.print(right);
		
		out.outdent();
		out.print(")");
	}

	@Override
	public IRExp build(List<IRExp> kids) {
		return new BINOP(binop, kids.get(0), kids.get(1));
	}

	@Override
	public List<IRExp> kids() {
		return List.list(left,right);
	}
/*  FIXME: disable interp
	@Override
	public Word interp(X86SimFrame env) {
		Word l = left.interp(env);
		Word r = right.interp(env);
		switch (binop) {
		case PLUS:
			return l.add(r);
		case MINUS:
			return l.minus(r);
		case MUL:
			return l.mul(r);
			//TODO: implement the other binops if you want to use them!
		default:
			throw new Error("Binop case missing? "+binop);
		}
	}
*/

	public Op getOp() {
		return binop;
	}
	
	

}

