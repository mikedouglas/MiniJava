package minijava.ast;

import org.junit.Assert;

public abstract class Expression extends AST {
	
	/**
	 * The type of an expression is set by the type checking phase.
	 */
	public Type type;

	public Type getType() {
		Assert.assertNotNull("Was this AST typechecked?", type);
		return type;
	}
	
	public void setType(Type theType) {
		Assert.assertNull(type);
		type = theType;
	}

}
