package minijava.ast;

import minijava.visitor.Visitor;

public class ArrayLength extends Expression {
	
	public final Expression array;
	
	public ArrayLength(Expression array) {
		super();
		this.array = array;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
