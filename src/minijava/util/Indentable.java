package minijava.util;

/**
 * Classes that like to produce indentable output can implement this
 * interface. This will make them look nicely formatted inside of
 * Tables (and other indentables).
 * <p>
 * Implementations of indentable should try to use the dump method to
 * indent nested elements if possible. If not possible they should fall
 * back on the nested element's toString method.
 * 
 * @author kdvolder
 */
public interface Indentable {
	
	void dump(IndentingWriter out);

}
