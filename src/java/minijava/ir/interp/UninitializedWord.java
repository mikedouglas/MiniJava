package minijava.ir.interp;

/**
 * The UninitializedWord represents a value that is unknown because
 * it is in a memory location that was never written to. 
 * <p>
 * Our implementation will throw an error if a program tries to operate
 * on an uninitialised word in any way.
 */
public class UninitializedWord extends Word {

	/**
	 * If something other than null is here, this will be used as the
	 * toString value for this UnitializedWord (to make the printout
	 * of stack frames a little more readable).
	 */
	private String info;
	
	public final static UninitializedWord the = new UninitializedWord("?");
	
	public UninitializedWord(String info) {
		this.info = info;
	}

	@Override
	public Word add(int value) {
		throw new Error("Operating performed on an unitialized word!");
	}
	
	@Override
	public String toString() {
		return info;
	}
	
}
