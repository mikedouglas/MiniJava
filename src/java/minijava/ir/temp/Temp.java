package minijava.ir.temp;

import junit.framework.Assert;

public class Temp  {
	private static int count;
	
	private Color color = null;

	private String name;
	
	public String toString() {
		return name;
	}
	public Temp() { 
		name="t"+count++;
	}
	
	/**
	 * Create a new Temp, which is pre-colored. This means the Temp
	 * represents an actual register. A new and unique color object is
	 * created to represent this register so that other temps can be
	 * colored with it.
	 */
	public Temp(String registerName) {
		this.name = registerName;
		this.color = new Color() {
			@Override
			public String toString() {
				return name;
			}

			@Override
			public boolean isRegister() {
				return true;
			}
		};
	}
	public Color getColor() {
		return color;
	}
	public void uncolor() {
		color = null;
	}
	
	/**
	 * A Temp can be painted in some color (this is used by the register allocator) to
	 * paint each Temp according to the register it is allocated to.
	 */
	public void paint(Color color) {
		Assert.assertNull("Not allowed to paint a Temp more than once! (Uncolor it first)", this.color );
		this.color = color;
	}
	
	public String getName() {
		return name;
	}
}

