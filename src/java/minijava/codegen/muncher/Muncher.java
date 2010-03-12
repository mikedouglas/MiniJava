package minijava.codegen.muncher;

import minijava.codegen.assem.Instr;
import minijava.ir.frame.Frame;
import minijava.ir.temp.Temp;
import minijava.ir.tree.IRExp;
import minijava.ir.tree.IRStm;
import minijava.util.DefaultIndentable;
import minijava.util.IndentingWriter;
import minijava.util.List;

public abstract class Muncher extends DefaultIndentable {
	
	/**
	 * Accumulates a list of instructions produced by emit.
	 */
	private List<Instr> instructions = List.empty();
	
	private MuncherRules<IRStm, Void> stmMunchers;
	private MuncherRules<IRExp, Temp> expMunchers;

	private Frame frame;
	
	protected Muncher(Frame frame, MuncherRules<IRStm, Void> stmMunchers, MuncherRules<IRExp, Temp> expMunchers) {
		this.frame = frame;
		this.stmMunchers = stmMunchers;
		this.expMunchers = expMunchers;
	}

	/**
	 * Add an instruction to the code. The new instruction is added to the
	 * end of the list.
	 */
	public void emit(Instr instr) {
		instructions.add(instr);
	}
	
	/**
	 * Munch the statement, adding instructions to this muncher's list of instructions
	 * as a side effect.
	 * <p>
	 * The implementation of this method should call "emit" to add instructions.
	 */
	public void munch(IRStm stm) {
		stmMunchers.munch(this, stm);
	}
	
	/**
	 * Like munch(IStm) but munches an IRExp. 
	 */
	public Temp munch(IRExp exp) {
		return expMunchers.munch(this, exp);
	}

	/**
	 * After the munching is complete, retrieve the accumulated instructions with this
	 * method.
	 */
	public List<Instr> getInstructions() {
		return instructions;
	}
	
	@Override
	public void dump(IndentingWriter out) {
		out.println("Muncher {");
		out.indent();
		
		out.print("stm ");
		out.println(stmMunchers);
		out.print("exp ");
		out.println(expMunchers);
		
		out.outdent();
		out.print("}");
	}
	
	public Frame getFrame() {
		return frame;
	}

}