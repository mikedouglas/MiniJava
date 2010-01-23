package minijava.util;

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * A helper class to produce indented output (e.g. for a pretty printer).
 * <p>
 * An instance of this class sends output to a wrapped PrintWriter and keeps
 * track of a level of indentation. 
 * It will ensures that an appropriate number of spaces is printed before any
 * actual output on each new line.
 * <p>
 * Note that newlines embedded inside of Strings are not processed by this
 * implementation. You should always use println to insert newlines into
 * the output.
 * 
 * @author kdvolder
 */
public class IndentingWriter {

	/**
	 * Current level of indentation.
	 */
	private int indentation = 0;
	
	/**
	 * This flag is true until we have printed something on a line.
	 * We use this to postpone printing of indentation spaces until something is
	 * actually printed on a line. This allows indentation level to be changed
	 * even after a newline has already been printed.
	 */
	private boolean startOfLine = true;
	
	/**
	 * Where to send the actual output to.
	 */
	PrintWriter out;
	
	public IndentingWriter(PrintWriter out) {
		this.out = out;
	}
	
	public IndentingWriter(StringWriter out) {
		this(new PrintWriter(out));
	}

	/**
	 * Close the wrapped PrintWriter.
	 */
	public void close() {
		out.close();
	}
	
	public void print(String string) {
		if (startOfLine) {
			startOfLine = false;
			for (int i = 0; i < indentation; i++) {
				print("   ");
			}
		}
		out.print(string);
	}
	public void println() {
		out.println();
		startOfLine = true;
	}

	/**
	 * Write an object to this IndentableWriter. If the object implements
	 * Indentable, then that implementation will be used. Otherwise we fall back
	 * on the object's toString method.
	 */
	public void print(Object obj) {
		if (obj instanceof Indentable) {
			Indentable iObj = (Indentable) obj;
			iObj.dump(this);
		}
		else {
			this.print(""+obj);
		}
	}

	public void println(Object obj) {
		print(obj);
		println();
	}

	public void indent() {
		indentation++;
	}
	public void outdent() {
		indentation--;
	}

}
