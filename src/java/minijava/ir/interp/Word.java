package minijava.ir.interp;

/**
 * Subclasses of this class represents words in memory. It is tempting to
 * implement this just as a wrapper around an int, but we want to be able
 * to simulate pointers to blocks of memory without actually implementing
 * our own memory manager. Thus our Words are essentially typed. They may 
 * be true integers or pointers. 
 * <p>
 * We will simulate pointer arithmethic appropriately. This has the added
 * benefit that we can detect when pointer arithmetic creates "out of bounds"
 * pointers.
 * 
 * @author kdvolder
 */
public abstract class Word {

	public final Word add(Word other) {
		if (this instanceof Int)
			return other.add(((Int)this).value);
		else if (other instanceof Int) {
			return this.add(((Int)other).value);
		}
		else
			throw new Error("add -- at least one of the operands must be an Int");
	}

	public abstract Word add(int value);

	public Word mul(Word r) {
		throw new Error("mul -- only supported on Int");
	}

	public int asInt() {
		throw new Error("Not an Int");
	}

	public Word minus(Word other) {
		//We reduce this to an addition.
		if (other instanceof Int)
			return this.add(-other.asInt());
		else
			throw new Error("minus -- operand types not supported "+this+" "+other);
		// Note: subtraction of two pointers that share the same base, is also 
		// meaningfull, it can be implemented by overriding this method in the 
		// appropriate pointer subclasses.
	}

	public boolean isLT(Word other) {
		throw new Error("lessThan -- not supported for "+this+" "+other);
	}

	/**
	 * Implements the EQ binop, but only if the rands are the same type of
	 * Word (code shouldn't really be comparing adresses to ints etc. with
	 * this operation!)
	 */
	public boolean isEQ(Word r) {
		throw new Error("EQ -- not supported for "+this+" "+r);
	}

}
