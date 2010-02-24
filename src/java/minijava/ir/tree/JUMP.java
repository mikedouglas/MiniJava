package minijava.ir.tree;

//import minijava.ir.interp.LabelPtr;
//import minijava.ir.interp.X86SimFrame;
import minijava.ir.temp.Label;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class JUMP extends IRStm {
	private IRExp exp;
	private List<Label> targets;
	public JUMP(IRExp e, List<Label> t) {exp=e; targets=t;}
	public JUMP(Label target) {
		this(new NAME(target), List.list(target));
	}
	@Override
	public void dump(IndentingWriter out) {
		out.print("JUMP(");
		out.print(exp);
		out.print(")");
	}
	@Override
	public List<IRExp> kids() {
		return List.list(getExp());
	}
	public IRExp getExp() {
		return exp;
	}
	@Override
	public IRStm build(List<IRExp> kids) {
		return new JUMP(kids.head(), targets);
	}
/*  FIXME: disabled interp
	@Override
	public Label interp(X86SimFrame env) {
		LabelPtr label = (LabelPtr) getExp().interp(env);
		return label.getLabel();
	}
*/
	@Override
	public boolean isJump() {
		return true;
	}
	@Override
	public List<Label> getJumpTargets() {
		return targets;
	}
}

