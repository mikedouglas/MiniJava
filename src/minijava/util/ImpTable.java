package minijava.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.Assert;

/**
 * This class can be used as a basic "imperative style" symbol table.
 * <p>
 * It is basically just a wrapper around a java.util.HashMap instance.
 * <p>
 * You can modify this implementation to suit your needs, or use it as
 * a building block in creating more complex table-like structures to represent
 * different kinds of MiniJava scopes.
 */
public class ImpTable<V> extends DefaultIndentable 
implements Iterable<Entry<String, V>>, Lookup<V>
{
	
	public static class DuplicateException extends Exception {

		private static final long serialVersionUID = 8650830167061316305L;

		public DuplicateException(String arg0) {
			super(arg0);
		}

	}

	private Map<String, V> map = new HashMap<String, V>();

	@Override
	public Iterator<Entry<String,V>> iterator() {
		return map.entrySet().iterator();
	}
	
	/**
	 * Insert an entry into the table. This modifies the table.
	 * Duplicate entries (i.e. with the same id are not supported).
	 * When a duplicate id is being entered a DuplicatException is
	 * thrown.
	 * <p>
	 * Note: the name "put" different from the name "insert" in the
	 * FunTable is deliberate. This will make sure you get compile
	 * errors when you try to change code using imperative puts into
	 * to use functional inserts instead.
	 */
	public void put(String id, V value) throws DuplicateException {
		Assert.assertNotNull(value);
		Assert.assertNotNull(id);
		V existing = map.get(id);
		if (existing!=null) throw new DuplicateException("Duplicate entry: "+id);
		map.put(id, value);
	}
	
	/**
	 * Find an entry in the table. If no entry is found, null is returned.
	 */
	public V lookup(String id) {
		return map.get(id);
	}

	@Override
	public void dump(IndentingWriter out) {
		out.println("Table {");
		out.indent();
		for (Entry<String, V> entry : this) {
			out.print(entry.getKey()+" = ");
			out.println(entry.getValue());
		}
		out.outdent();
		out.print("}");
	}

	public boolean isEmpty() {
		return size()==0;
	}

	@Override
	public int size() {
		return map.size();
	}

}
