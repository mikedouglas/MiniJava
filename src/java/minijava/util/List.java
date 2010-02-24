package minijava.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * An "immutable" List interface.
 * <p>
 * The list actually provides a method for adding elements
 * in a destructive way. These are intended to be used only to initialize
 * the list. As soon as the end (or size) of the list is observed, the
 * list is made immutable and further add operations are
 * prohibited.
 * <p>
 * This results in a kind of practical immutability: The list will appear immutable
 * to any observer, while mutation operations can safely be used during the initialization of
 * the list, since it is guaranteed that observers will only every see one version of the list.
 * <p>
 * There can be multiple classes implementing this interface. These can
 * be used interchangeably and together with one another.
 * <p>
 * For example, we provide both a linked list implementation as well as
 * a ArrayList style implementation. It is possible to take the tail / head
 * of array lists or cons to array lists. Both these operations are efficient
 * (O(1) in time / memory).
 * <p>
 * Consing to an array list implementation results in a "hybrid" linked+array 
 * list, consisting of a cons pair who's tail pointer points to an array list.
 * <p>
 * The subclasses of this class are not public. One is only supposed to use the
 * methods in this class to create list instances.
 */
public abstract class List<E> extends DefaultIndentable implements Iterable<E> {

	public abstract E get(int i);

	public abstract int size();

	public abstract void add(E e);

	/**
	 * We provide this default implementation to create an iterator. It is
	 * efficient under the assumption that the "tail" operation is implemented
	 * efficiently (i.e. without copying the entire tail end of the list into 
	 * a new data structure). This is the case for both our Array and linked
	 * list based implementation.
	 */
	public Iterator<E> iterator() {
		return new Iterator<E>() {
			
			private List<E> rest = List.this;

			@Override
			public boolean hasNext() {
				return !rest.isEmpty();
			}

			@Override
			public E next() {
				E head = rest.head();
				rest = rest.tail();
				return head;
			}

			@Override
			public void remove() {
				throw new Error("Can't remove, this is an immutable data structure");
			}
			
		};
	}

	public final E head() { return get(0); }

	public abstract List<E> tail();

	public abstract boolean isEmpty();
	
	public boolean contains(E what) {
		for (E e : this)
			if (e.equals(what)) return true;
		return false;
	}

	/**
	 * Create a new empty list, which is still mutable (so elements can still be
	 * added to it).
	 */
	@SuppressWarnings("unchecked")
	public static <E> List<E> empty() {
		return List.list();
	}
	
	/**
	 * An immutable empty list. The very same object is returned any time
	 * this method is called. It is safe to use this as a list of any type of
	 * element, since it doesn't contain any elements, and its add method is
	 * disabled!
	 */
	@SuppressWarnings("unchecked")
	public static <E> List<E> theEmpty() {
		return theEmpty;
	}
	
	/**
	 * An immutable empty list.
	 */
	@SuppressWarnings("unchecked")
	private static final List theEmpty = new List() {
		@Override
		public void add(Object e) {
			throw new Error("This list is immutable");
		}

		@Override
		public Object get(int i) {
			throw new NoSuchElementException("Empty list has no elements");
		}

		@Override
		public boolean isEmpty() {
			return true;
		}

		@Override
		public int size() {
			return 0;
		}

		@Override
		public List tail() {
			throw new IllegalArgumentException("Empty list has no tail");
		}
	};

	public static <E> List<E> cons(E car, List<E> cdr) {
		return new ConsList<E>(car, cdr);
	}

	public static <E> List<E> list(E... elements) {
		return new ArrList<E>(elements);
	}
	
	@Override
	public void dump(IndentingWriter out) {
		out.println("List {");
		out.indent();
		
		for (E e : this) 
			out.println(e);
		
		out.outdent();
		out.print("}");
	}

	public E getLast() {
		return get(size()-1);
	}

	public List<E> reverse() {
		List<E> reversed = List.empty();
		for (E e : this) {
			reversed = cons(e, reversed);
		}
		return reversed;
	}

	/**
	 * A "functional" append: returns a new list, doesn't
	 * modify original lists.
	 */
	public List<E> append(List<E> more) {
		if (this.isEmpty()) 
			return more;
		else
			return cons(head(), tail().append(more));
	}

	/**
	 * Delete the first occurrence of an element. Throws an
	 * error if the element is not found.
	 * <p>
	 * This is a "functional" delete: returns a new list, doesn't
	 * modify original list.
	 */
	public List<E> delete(E e) {
		if (isEmpty()) 
			throw new IllegalArgumentException("List.delete -- Element not in the list");
		if (e.equals(head()))
			return tail();
		else
			return cons(head(), tail().delete(e));
	}

	/**
	 * Delete all occurrences of an element. If the element is
	 * not found, returns an identical copy of the list.
	 * <p>
	 * This is a "functional" delete: returns a new list, doesn't
	 * modify original list.
	 */
	public List<E> deleteAll(E e) {
		if (isEmpty()) 
			return this;
		else if (e.equals(head()))
			return tail().deleteAll(e);
		else
			return cons(head(), tail().deleteAll(e));
	}

	public List<E> union(List<E> add) {
		List<E> result = this;
		for (E e : add) {
			if (!result.contains(e))
				result = cons(e, result);
		}
		return result;
	}

}