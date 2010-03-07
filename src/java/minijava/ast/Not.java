package minijava.ast;

import minijava.visitor.Visitor;

public class Not extends Expression {

	public final Expression e;
	
	public Not(Expression e) {
		super();
		super.setType(BooleanType.instance);
		this.e = e;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
