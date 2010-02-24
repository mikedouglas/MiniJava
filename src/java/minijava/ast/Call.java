package minijava.ast;

import minijava.visitor.Visitor;

public class Call extends Expression {

	public final Expression receiver;
	public final String name;
	public final NodeList<Expression> rands;

	public Call(Expression receiver, String name, NodeList<Expression> rands) {
		super();
		this.receiver = receiver;
		this.name = name;
		this.rands = rands;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
