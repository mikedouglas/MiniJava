package minijava.codegen.patterns;

import static minijava.util.List.cons;
import minijava.util.DefaultIndentable;
import minijava.util.List;

/**
 * Rather than write clunky and possibly error prone code full of instanceof tests, in the
 * implementation of instruction selection, we will implement some infrastructure to do a 
 * rudimentary form of pattern matching on IR code.
 * <p>
 * The implementation of the pattern matching methods will perhaps be clunky, but at least
 * this will hide away that clunkyness inside of the matching infrastructure.
 * <p>
 * For some flexibility in allowing complex patterns that may match different number of 
 * tiles (e.g. to support complex MEM patterns for X86 assembly) not all patterns
 * are required to implement the "size" method. 
 * <p>
 * Patterns that do not implement the size method, however, must implement an "explode" 
 * method that explodes this pattern into a list of alternative patterns that are
 * less complex.
 * <p>
 * It is assumed that any pattern can be exploded recursively until the exploded patterns
 * are simple enough to be patterns of a "fixed" size.
 *
 * @author kdvolder
 */
public abstract class Pat<N> extends DefaultIndentable {

	/**
	 * This method is the one intended for "clients" to call to request matching a
	 * pattern to a Node. 
	 * <p>
	 * This method should not be called recursively or children will be lost. 
	 * @param toMatch
	 * @return null if the match failed or a Matched "map" if the match succeeded.
	 */
	public Matched tryMatch(N toMatch) {
		Matched m = new Matched();
		try {
			match(toMatch, m);
			return m;
		} catch (ClassCastException e) {
		} catch (Failed e) {}
		return null;
	}
	
	/**
	 * Retrieve a list of this pattern's sub patterns. Returns an empty list if this
	 * is a "leaf" pattern.
	 * @throws VariableSizeException 
	 */
	public abstract List<Pat<?>> children();
	
	/**
	 * Make a pattern just like this one, but replacing it's children.
	 * @throws VariableSizeException 
	 */
	public abstract Pat<N> build(List<Pat<?>> children);

	/**
	 * Computes the size of a pattern. I.e. the number of nodes the pattern covers in the IR tree.
	 * <p>
	 * Patterns that are too complex to be able to compute a single "fixed" size, should throw an
	 * UnsupportedOperationException, and they should implement the explode method, to reduce this pattern
	 * to a list of less complex alternative patterns.
	 * <p>
	 * Note: a typical implementation of this method is 1+SUM(size(each sub pattern))
	 */
	public int size() {
		int sum = 1;
		for (Pat<?> p : children()) {
			sum += p.size();
		}
		return sum;
	}
	
	/**
	 * Explode a complex pattern into a list of alternative patterns of smaller complexity.
	 * <p>
	 * Patterns that do not implement the size method must override this trivial default implementation	
	 */
	@SuppressWarnings("unchecked")
	public
	List<Pat<N>> explode() {
		if (isFixedSize())
			return List.list(this);
		else {
			List<Pat<?>> children = children();
			List<Pat<N>> exploded = List.empty();
			List<Pat<?>> explChildren = List.empty();
			explode(children, explChildren, exploded);
			return exploded;
		}
	}

	/** Helper method to recurse over a list of children and explode each */ 
	private void explode(List<Pat<?>> rest, List<Pat<?>> exploded, List<Pat<N>> result) {
		if (rest.isEmpty()) {
			result.add(build(exploded.reverse()));
		}
		else {
			List<? extends Pat<?>> head = rest.head().explode();
			for (Pat<?> pat : head)
				explode(rest.tail(), cons(pat, exploded), result);
		}
	}

	/**
	 * This method should only be called recursively by another match methods. To make the matcher
	 * methods easier to implement, we will catch any {@link ClassCastException} and treat it as a
	 * failure to match. 
	 * 
	 * @param toMatch
	 * @param children a List which should be initially empty to accumulate the children of a matched pattern.
	 * @throws Failed
	 */
	public abstract void match(N toMatch, Matched matched) throws Failed, ClassCastException;
	
	public static class Failed extends Exception {
		private static final long serialVersionUID = 1L;
	}

	//// Methods for creating patterns:
	
	/**
	 * Pattern that matches anything.
	 */
	public static <N> Pat<N> any() {
		return new Wildcard<N>();
	}
		
	protected void fail() throws Failed {
		throw new Failed();
	}

	public boolean isFixedSize() {
		try { size(); return true; }
		catch (UnsupportedOperationException e) {
			return false;
		}
	}

}
