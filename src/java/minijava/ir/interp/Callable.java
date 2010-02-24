package minijava.ir.interp;

import minijava.util.List;

/**
 * Represents a subroutine that can be invoked by a IR.CALL instruction. 
 * <p>
 * This could be a wrapper around a MethodFragment, or a special "system"
 * function.
 * <p>
 * This is a subclass of Word, since words can hold (pointers to) procedures
 */
public abstract class Callable extends Word {
	
	public abstract Word call(Interp interp, List<Word> list);
	
	@Override
	public Ptr add(int value) {
		throw new Error("add -- doesn't make sense on procedure addresses");
	}
}

