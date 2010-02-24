package minijava.ir.temp;

/**
 * An instance of this class represent a "color" that can be used to
 * paint Temps. This is used by the register allocation phase.
 * <p>
 * We only provide an abstract class here, so that various algorithms
 * can choose to implement different kinds of Colors.
 * <p>
 * For example, you may want to create a special color to paint 
 * spilled Temp's with in the register allocator, so you can store
 * information about them, such as their location in the frame, in their 
 * paint color.
 */
public abstract class Color {
	
	@Override
	public abstract String toString();
	
	/**
	 * If this returns true, then a Temp with this color should be
	 * treated as a register by the IR patterns. 
	 * <p>
	 * An IRPat.TEMP(t) pattern will only match if a Temp is unpainted, 
	 * or painted with a register color.
	 */
	public abstract boolean isRegister();
	
}
