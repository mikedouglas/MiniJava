package minijava.ast;

import minijava.visitor.Visitor;

public class Print extends Statement {

	public final Expression exp;

	public Print(Expression exp) {
		super();
		this.exp = exp;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
