package minijava.ir.temp;

import java.util.HashMap;
import java.util.Map;

/**
 * A Label represents an address in assembly language.
 * <p>
 * This implementation of labels is a little more sophisticated than the
 * one in the book. 
 * <p>
 * The constructors have been made private. You should call the static
 * methods of this class to create labels. (This makes it easier to evolve
 * the Label class without having to change all the code that uses it.
 * Also, it is important to distinguish the "get" from the "generate" method,
 * which would be hard with constructors since they have the same parameter name. 
 */
public class Label  {
	
	/**
	 * A map for use in the fromString method (to return the same Label object if a 
	 * label with this name already exists).
	 */
	private static Map<String, Label> labels = new HashMap<String, Label>();
	
	private String name;
	private static int count;

	/**
	 * a printable representation of the label, for use in assembly 
	 * language output.
	 */
	@Override
	public String toString() {return name;}

	/**
	 * Makes a new unique label with a generated name. 
	 * The debugName provided as an argument will be used as part
	 * of the generated name, to aid in debugging/reading IR code. 
	 */
	private Label(String printsAs) {
		name=printsAs;
	}

	/**
	 * Makes a new (unique) label with a generated name.
	 */
	public static Label gen() {
		return generate("L");
	}

	/**
	 * Generate a new label. The string provided as an argument
	 * will be incorporated into the generated label name (to aid in
	 * debugging).
	 */
	public static Label generate(String debugName) {
		return new Label(debugName+"_"+count++);
	}

	/**
	 * Retrieve the label with a given name. A new label will be created
	 * only if a label with this name does not yet exist.
	 */
	public static Label get(String name) {
		Label existing = labels.get(name);
		if (existing!=null) return existing;
		Label newLabel = new Label(name);
		labels.put(name, newLabel);
		return newLabel;
	}	

}
