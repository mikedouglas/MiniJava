package minijava.ir.tree;

//import minijava.ir.interp.Int;
//import minijava.ir.interp.Word;
//import minijava.ir.interp.X86SimFrame;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class CONST extends IRExp {
	private int value;
	public CONST(int v) {value=v;}
	@Override
	public void dump(IndentingWriter out) {
		out.print("CONST ");
		out.print(value);
	}
	
	@Override
	public IRExp build(List<IRExp> kids) {
		return this;
	}
	@Override
	public List<IRExp> kids() {
		return List.empty();
	}
//	@Override
//	public Word interp(X86SimFrame env) {
//		return new Int(value);
//	}
	public int getValue() {
		return value;
	}
	
	@Override
	public boolean isCONST(int i) {
		return i == value;
	}
}

