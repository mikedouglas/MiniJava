package minijava.codegen.assem;

import minijava.ir.temp.Label;
import minijava.ir.temp.Temp;
import minijava.ir.tree.LABEL;
import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * Represents an assembly language LABEL.
 * <p>
 * Note: renamed from the Book code to make it distinct from
 * the LABEL class in the ir package.
 */
public class A_LABEL extends Instr {
	private Label label;

	public A_LABEL(String a, Label l) {
		super(a); 
		label=l;
	}

	public A_LABEL(String a, LABEL l) {
		this(a, l.getLabel());
	}

	@Override
	public List<Temp> use()    {return List.empty();}
	@Override
	public List<Temp> def()    {return List.empty();}
	@Override
	public List<Label> jumps() {return null;}

	public Label getLabel() {
		return label;
	}
	
	@Override
	public void dump(IndentingWriter out) {
		out.outdent();
		super.dump(out);
		out.indent();
	}

}
