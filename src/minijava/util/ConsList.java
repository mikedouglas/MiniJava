package minijava.util;

import junit.framework.Assert;

/**
 * An immutable pair that represents the Cons of an element
 * to the front of another List.
 * <p>
 * Adding to a cons list, while supported is currently not
 * implemented efficiently O(n). 
 * 
 * @author kdvolder
 */
class ConsList<E> extends List<E> {
	
	private final E car;
	private final List<E> cdr;

	ConsList(E car, List<E> cdr) {
		Assert.assertNotNull(cdr);
		this.car = car;
		this.cdr = cdr;
	}

	@Override
	public void add(E e) {
		cdr.add(e);
	}

	@Override
	public E get(int i) {
		if (i==0) return car;
		else return cdr.get(i-1);
	}

	@Override
	public boolean isEmpty() {
		return false;
	}

	@Override
	public int size() {
		return 1+cdr.size();
	}

	@Override
	public List<E> tail() {
		return cdr;
	}

}
