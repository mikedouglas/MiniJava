package minijava.util;

import java.io.StringWriter;

/**
 * If you have no other superclass and want to implement Indentable, you can
 * use this as a superclass. This will give you a default implementation of
 * the toString method that calls the Indentable implementation.
 * <p>
 * If you do not derive your Indentable class from this superclass, then
 * you should probably copy this class's toString method over.
 * 
 * @author kdvolder
 */
public abstract class DefaultIndentable implements Indentable {

	@Override
	public String toString() {
		StringWriter out = new StringWriter();
		this.dump(new IndentingWriter(out));
		return out.toString();
	}

}
