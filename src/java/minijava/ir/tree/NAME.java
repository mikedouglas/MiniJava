package minijava.ir.tree;
//import minijava.ir.interp.Word;
//import minijava.ir.interp.X86SimFrame;
import minijava.ir.temp.Label;
import minijava.util.IndentingWriter;
import minijava.util.List;
public class NAME extends IRExp {
	private Label label;
	public NAME(Label l) {label=l;}
	@Override
	public void dump(IndentingWriter out) {
		out.print("NAME(");
		out.print(label);
		out.print(")");
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
//		return env.getLabel(label);
//	}
	public Label getLabel() {
		return label;
	}
	
}

