package minijava.util;

import java.util.Iterator;
import java.util.NoSuchElementException;


/**
 * This corresponds to the Table class in the book. It can be used as a
 * "functional style" symbol table.
 * <p>
 * The fact that this code is included in the project drop does not imply that
 * you are required to use it. Indeed, you must decide between a functional
 * and imperative style table (or a combination of both). You may use this
 * as a "functional style" implementation, or you can use ImpTable instead.
 * Or you can use something else entirely.
 * <p>
 * The implementation here is more complex than the one in the book, but it
 * provides a much cleaner abstraction of how the table class is internally
 * represented. All functionality of tables is accessible only via public
 * methods in the class FunTable. This makes it easy for the implementation of the 
 * table class to be replaced with a better, more optimised one (e.g. as 
 * described in exercise 1.1 of the book) without altering the rest of the code.
 * <p>
 * The current implementation is not particularly efficient since it uses a linearly
 * linked list of entries. You may consider implementing a more efficient version of
 * this that uses some kind of tree data structure internally to efficiently represent
 * a functional style symbol table.
 * 
 * @author kdvolder
 */
public abstract class FunTable<V> extends DefaultIndentable
implements Lookup<V>, Iterable<FunTable.Entry<V>>  {
	
	FunTable() { super(); }

	/**
	 * Dump contents of this table in nicely indented format.
	 */
	public void dump(IndentingWriter out) {
		out.println("Table {");
		out.indent();
		for (Entry<V> entry : this) {
			out.print(entry.getId()+" = ");
			out.print(entry.getValue());
			out.println();
		}
		out.outdent();
		out.print("}");
	}

	/**
	 * An instance of table that represents the EmptyTable.
	 */
	public static final <V> FunTable<V> theEmpty() {return new EmptyTable<V>(); }
	
	/**
	 * Extend a table with a new Table Entry associating an id to its
	 * value. This does not modify the original table but returns a new
	 * "extended" table.
	 */
	public FunTable<V> insert(String id, V value) {
		return new Entry<V>(id, value, this);
	}
	
	/**
	 * Finds the value of an Entry in a table. Returns null if no corresponding entry
	 * exists in the table.
	 */
	public V lookup(String id) {
		//Note: the book suggests a recursive implementation for this
		//function, which is also a good way to implement it.
		//However, in Java there is no proper tail call optimisation and
		//so recursive implementations can make large, unnecessary demand 
		//on the stack. This implementation makes use of our implementation
		//Iterable and is equally readable yet more efficient 
		//(no unbounded stack growth).
		for (Entry<V> entry : this)
			if (entry.getId().equals(id))
				return entry.getValue();
		return null;
	}

	/**
	 * Returns true if and only if this table is empty.
	 */
	public abstract boolean isEmpty();
	

	/**
	 * An Entry in a table. Note that the constructor is not public.
	 * You should use the "insert" method of Table to create table entries.
	 * <p>
	 * Note also that the "tail" pointer is not accessible to user code.
	 * It is private and we have not provided an accessor for it. The fact
	 * that the table is implemented as a "linked list" should be hidden
	 * from user code!
	 */
	public static class Entry<V> extends FunTable<V> {

		private String id;
		private V value;
		private FunTable<V> tail;

		private Entry(String id, V value, FunTable<V> table) {
			this.id = id;
			this.value = value;
			this.tail = table;
		}
		
		@Override
		public boolean isEmpty() {
			return false;
		}

		public String getId() {
			return id;
		}

		public V getValue() {
			return value;
		}

		@Override
		public FunTable<V> merge(FunTable<V> superEntries) {
			FunTable<V> rest = tail.merge(superEntries);
			return new Entry<V>(id, value, rest);
		}

	}

	@Override
	public Iterator<Entry<V>> iterator() {
		return new TableIterator(this);
	}

	public int size() {
		int count = 0;
		for (@SuppressWarnings("unused") Entry<V> entry : this) count++;
		return count;
	}

	/**
	 * Merge two tables together into a new table.
	 * <p>
	 * The order of entries in both tables is preserved and the
	 * entries if the leftmost table take priority over the rightmost
	 * table if they have the same id.
	 */
	public abstract FunTable<V> merge(FunTable<V> superEntries);

	///////////// all code below here is strictly private ///////////////////
	
	/**
	 * A class to represent an empty table. This class is a singleton:
	 * there is only one instance of EmptyTable. 
	 */
	private static class EmptyTable<V> extends FunTable<V> {
		@Override
		public String toString() {
			return "Table{}";
		}

		@Override
		public boolean isEmpty() {
			return true;
		}

		@Override
		public FunTable<V> merge(FunTable<V> superEntries) {
			return superEntries;
		}
	}
	
	private class TableIterator implements Iterator<Entry<V>> {

		private FunTable<V> tablePtr;

		private TableIterator(FunTable<V> table) {
			this.tablePtr = table;
		}

		@Override
		public boolean hasNext() {
			return !tablePtr.isEmpty();
		}

		@Override
		public Entry<V> next() {
			if (!hasNext())
				throw new NoSuchElementException();
			else {
				Entry<V> entry = (Entry<V>) tablePtr;
				tablePtr = entry.tail;
				return entry;
			}
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException("Tables are immutable, can not remove elements");
		}

	}

}
