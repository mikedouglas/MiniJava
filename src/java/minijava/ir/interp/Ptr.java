package minijava.ir.interp;

/**
 * A type of word that points to some data in memory.
 */
public abstract class Ptr extends Word {

	public abstract void set(Word newValue);
	public abstract Word get();
	
	@Override
	public abstract Ptr add(int bytesOffset);
	
}
