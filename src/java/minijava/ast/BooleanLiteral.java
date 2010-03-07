package minijava.ast;

import minijava.visitor.Visitor;

public class BooleanLiteral extends Expression {
	
	public final boolean value;

	public BooleanLiteral(boolean value) {
		super();
		super.setType(BooleanType.instance);
		this.value = value;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
