package minijava.ir.frame;

//import minijava.codegen.assem.Instr;
//import minijava.codegen.muncher.Muncher;
//import minijava.ir.interp.Interp;
//import minijava.ir.interp.Word;
//import minijava.ir.interp.X86SimFrame;
import minijava.ir.temp.Label;
//import minijava.ir.temp.Temp;
import minijava.ir.tree.IRExp;
import minijava.ir.tree.IRStm;
import minijava.util.DefaultIndentable;
//import minijava.util.IndentingWriter;
import minijava.util.List;


public abstract class Frame extends DefaultIndentable {
	
	/** 
	 * Create a new frame for a the target architecture and
	 * allocate space for the formal parameters according to the target architecture's
	 * calling conventions. 
	 */
	public abstract Frame newFrame(Label name, List<Boolean> formalsEscape);
	
	private Label label;
	private List<Access> formals;
	
	protected Frame(Label label, List<Access> formals) {
		this.label = label;
		this.formals = formals;
	}

	public final Label getLabel() { return label; }
	
	public final List<Access> getFormals() {
		return formals;
	}
	public Access getFormal(int i) {
		return getFormals().get(i);
	}
	
	/**
	 * Allocate space for a local variable in this frame.
	 */
	public abstract Access allocLocal(boolean escapes);
	
	///////////////////////////////////////////////////////////////////
	/// Chapter 7 additions: (for translation to IR phase)

	/** Frame pointer (e.g. a temp mapped to %ebp on x86) */
	public abstract IRExp FP();
	
	/** Return value (e.g. a temp mapped to %eax on x86) */
	public abstract IRExp RV();
	
	/**
	 * Return the natural wordSize for the target architecture.
	 */
	public abstract int wordSize();

	/**
	 * Not really sure what this is supposed to do (according to the book).
	 *
	 * My implementation right now wraps code to save / restore callee save 
	 * registers around the procedure body.
	 */
	public abstract IRStm procEntryExit1(IRStm body);
	
	/**
	 * Create an object to simulate the contents of this Frame, used by the
	 * IR interpreter to simulate the execution of IR code for testing purposes.
	 */
	// FIXME: disabled interp
	//public abstract X86SimFrame newSimFrame(Interp interp, List<Word> args);

}
