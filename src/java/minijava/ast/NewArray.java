package minijava.ast;

import minijava.visitor.Visitor;

public class NewArray extends Expression {

	public final Expression size;

	public NewArray(Expression size) {
		super();
		super.setType(IntArrayType.instance);
		this.size = size;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
