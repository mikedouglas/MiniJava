package minijava.ast;

import minijava.visitor.Visitor;

public class NewObject extends Expression {

	public final String typeName;

	public NewObject(String typeName) {
		super();
		this.typeName = typeName;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
