package minijava.ir.tree;

//import minijava.ir.interp.X86SimFrame;
import minijava.ir.temp.Label;
import minijava.util.DefaultIndentable;
import minijava.util.List;

abstract public class IRStm extends DefaultIndentable implements IRNode {
	//The book implements these methods on most statements
	abstract public List<IRExp> kids();
	abstract public IRStm build(List<IRExp> kids);
	
	/**
	 * Simulate the execution of an atomic IRStm. 
	 * The statement may return a Label
	 * if its execution causes an explicit transfer of control, or it may return null
	 * for an implicit transfer of control to the next statement.
	 */
	// FIXME: removed interp ability
	//public abstract Label interp(X86SimFrame frame);
	
	/**
	 * Is this particular statement a kind of jump (to be more precise,
	 * is it a statement that transfers control explicitly rather than 
	 * "fall" into the next statement implicitly.
	 */
	public boolean isJump() {
		return false; 
		   // Most statements aren't jumps. We must not forget to override
		   // this method for those that are!
	}
	
	/**
	 * If this statement is a kind of JUMP, then this method should be
	 * implemented and it should return a List of possible jump targets.
	 * <p>
	 * This method is used by the TraceScheduling algorithm. The order
	 * in which the jumptargets are retur
	 */
	public List<Label> getJumpTargets() {
		throw new Error("Not implemented (not a JUMP statement)");
	}
	
}

