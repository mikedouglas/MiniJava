package minijava.util;

/**
 * This interface is implemented by both functional and imperative tables. Essentially, they
 * can both be used to look things up.
 */
public interface Lookup<V> {
	
	/**
	 * Lookup an identifier's value in the table. Returns null if no corresponding entry is found.
	 */
	V lookup(String id);

	int size();
	
	boolean isEmpty();
}
