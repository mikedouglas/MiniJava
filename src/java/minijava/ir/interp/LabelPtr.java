package minijava.ir.interp;

import minijava.ir.temp.Label;

/**
 * Represents the address of a Label that can serve as a
 * JUMP target.
 * <p>
 * Although this is a kind of pointer it is not a subclass of
 * Ptr because it doesn't support the same operations as a pointer
 * to data.
 * 
 * @author kdvolder
 */
public class LabelPtr extends Word {
	
	private final Label label;

	public LabelPtr(Label label) {
		this.label = label;
	}

	@Override
	public Word add(int value) {
		throw new Error("No address arithmetic on code pointers!");
	}

	public Label getLabel() {
		return label;
	}

}
