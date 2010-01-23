package minijava.ast;

import minijava.visitor.Visitor;

public class ArrayAssign extends Statement {

	public final String name;
	public final Expression index;
	public final Expression value;
	
	public ArrayAssign(String name, Expression index, Expression value) {
		super();
		this.name = name;
		this.index = index;
		this.value = value;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
