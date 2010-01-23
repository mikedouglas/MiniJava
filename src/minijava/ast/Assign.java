package minijava.ast;

import minijava.visitor.Visitor;

public class Assign extends Statement {
	
	public final String name;
	public final Expression value;
	
	public Assign(String name, Expression value) {
		super();
		this.name = name;
		this.value = value;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
