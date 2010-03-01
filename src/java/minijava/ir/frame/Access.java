package minijava.ir.frame;

import minijava.ir.tree.IRExp;


/**
 * An instance of this class represents a place to store a local, temp or parameter.
 * <p>
 * It may be a register or memory address relative to the current stack frame. 
 * <p>
 * This class is abstract for two reasons. First, there are concrete subclasses
 * for the different cases (at least two: one for register and one for inframe). 
 * Second, each architecture can provide its own concrete implementation (which can be 
 * dependent on specifics of the target machine or assembly language).
 */
public interface Access {
	
	/**
	 * Convert into some convenient String representation.
	 */
	@Override
	public abstract String toString();

	/**
	 * Translate into intermediate representation.
	 */
	public abstract IRExp exp(IRExp fp);
	
}
