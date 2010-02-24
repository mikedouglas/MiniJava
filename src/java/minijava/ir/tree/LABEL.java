package minijava.ir.tree;
//import minijava.ir.interp.X86SimFrame;
import minijava.ir.temp.Label;
import minijava.util.IndentingWriter;
import minijava.util.List;
public class LABEL extends IRStm { 
	private Label label;
	public LABEL(Label l) {label=l;}
	@Override
	public void dump(IndentingWriter out) {
		out.print("LABEL ");
		out.print(label);
	}
	@Override
	public IRStm build(List<IRExp> kids) {
		return this;
	}
	@Override
	public List<IRExp> kids() {
		return List.empty();
	}
	public Label getLabel() {
		return label;
	}
/*  FIXME: disabled interp
	@Override
	public Label interp(X86SimFrame frame) {
		//Label's aren't real instructions. They don't *do* anything.
		return null;
	}
*/
}

