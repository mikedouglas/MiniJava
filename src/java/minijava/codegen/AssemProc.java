package minijava.codegen;

import minijava.codegen.assem.Instr;
import minijava.codegen.muncher.Muncher;
import minijava.ir.frame.Frame;
import minijava.ir.temp.Label;
import minijava.ir.tree.IRStm;
import minijava.translate.ProcFragment;
import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * A class that represents the assembly code for a procedure. Since this is
 * specific to target architecture, this class may need subclasses
 * for different target architectures.
 * <p>
 * This implementation provided prints out procedure sequence that will work
 * for GAS (the GNU assembler). This should be portable for different target
 * architectures (since GAS is).
 * 
 * @author kdvolder
 */
public class AssemProc extends AssemFragment {
	
	private Frame frame;
	private ProcFragment procIR;
	private List<Instr> asmBody;

	public AssemProc(ProcFragment procIR) {
		this.frame = procIR.getFrame();
		this.procIR = procIR;
		rewrite(); // Actually not "re" writing, but writing for the first time :-)
	}

	@Override
	public void dump(IndentingWriter out) {
		out.println(".globl "+getLabel());
		out.println("     .type "+getLabel()+", @function");
		out.println(getLabel()+":");
		out.indent();
		
		frame.entrySequence(out);
		for (Instr instr : getBody()) {
			out.println(instr);
		}
		frame.exitSequence(out);
		
		out.println(".size "+getLabel()+", .-"+getLabel());
		
		out.outdent();
	}


	public Label getLabel() {
		return frame.getLabel();
	}

	public List<Instr> getBody() {
		return asmBody;
	}

	public Frame getFrame() {
		return frame;
	}
	
	/**
	 * After doing register allocation with spilled registers. You can use
	 * this method to rewrite the instructions in the body from the IR. 
	 * <p>
	 * To make this actually work, you will need to do something to define
	 * proper code generation rules to handle spilled Temps.
	 * <p>
	 * (At least) two options are available:
	 *  - define special patterns and rules to match spilled temps explicitly.
	 *  - alter the implementation of MEMPat to allow it to match not just
	 *  MEM nodes but also spilled Temp nodes.
	 */
	public void rewrite() {
		List<IRStm> body = procIR.getTraceScheduledBody();
		Muncher m = frame.newMuncher();
		for (IRStm stm : body) {
			m.munch(stm);
		}
		this.asmBody = frame.procEntryExit2(m.getInstructions());
	}

}
