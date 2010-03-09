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
		//Assert.assertNull(type); 
		//As our visitor is written, we expect the types of some nodes to be set multiple times.
		//as long as there is no conflict in these type settings, there is no problem, and the assertion has been
		//modified to reflect that.
		assert(type==null || type.equals(theType));		
		type = theType;
	}

}
