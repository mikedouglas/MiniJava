package minijava.util;

import java.util.ArrayList;

import junit.framework.Assert;

/**
 * An implementation of the {@link List} interface that uses an ArrayList
 * to store elements.
 * <p>
 * This class is not public. The idea is to only use the static methods in
 * the {@link List} interface to create List instances. Depending on what
 * methods are used to construct the list different types of representations
 * will result.
 */
class ArrList<E> extends List<E> implements Iterable<E>  {


	private boolean allowMutation = true;
	private java.util.List<E> elements;
	
	/**
	 * @param elements
	 */
	ArrList(E... elements) {
		this.elements = new ArrayList<E>(elements.length);
		for (int i = 0; i < elements.length; i++) {
			this.elements.add(elements[i]);
		}
	}
	
	/**
	 * This constructor is like a "scheme cons".  Be warned that
	 * it copies the second list and is inefficient if that list is
	 * long.
	 */
	ArrList(E car, List<E> cdr) {
		elements = new ArrayList<E>(cdr.size()+1);
		elements.add(car);
		for (E e : cdr) {
			elements.add(e);
		}
	}

	/* (non-Javadoc)
	 * @see minijava.util.List#get(int)
	 */
	public E get(int i) {
		return elements.get(i);
	}

	/* (non-Javadoc)
	 * @see minijava.util.List#size()
	 */
	public int size() {
		makeImmutable(); 
		return elements.size();
	}

	/* (non-Javadoc)
	 * @see minijava.util.List#add(E)
	 */
	public void add(E e) {
		Assert.assertTrue(allowMutation);
		elements.add(e);
	}

	/**
	 * Make this list immutable from now on (so the add method will
	 * stop working).
	 */
	public void makeImmutable() {
		//allowMutation = false;
	}

	/* (non-Javadoc)
	 * @see minijava.util.List#tail()
	 */
	public List<E> tail() {
		return new TailList<E>(this, 1); 
	}
	private static class TailList<E> extends List<E> {

		/**
		 * The list of which this is a "tail"
		 */
		private ArrList<E> from;
		
		/**
		 * The number of elements to drop from the front of the list.
		 */
		private int drop;

		public TailList(ArrList<E> from, int drop) {
			Assert.assertTrue(drop>0);
			// Checking this:
			// Assert.assertTrue(drop<=from.size());
			// would observe the size of the from array and make it
			// immutable. Not checking it may will postpone potential errors, 
			// but it will allow the creation of a "TailList" on a list that may 
			// only become fully initialised later (This should be ok, as long as
			// the elements of the tail become available before they are requested
			// from the tail.
			this.from = from;
			this.drop = drop;
		}

		@Override
		public void add(E e) {
			throw new Error("This list is immutable!");
		}

		@Override
		public E get(int i) {
			return from.get(i+drop);
		}

		@Override
		public boolean isEmpty() {
			return size()==0;
		}

		@Override
		public int size() {
			return from.size()-drop;
		}

		@Override
		public List<E> tail() {
			return new TailList<E>(from, drop+1);
		}

	}

	/* (non-Javadoc)
	 * @see minijava.util.List#isEmpty()
	 */
	public boolean isEmpty() {
		return size()==0;
	}

}