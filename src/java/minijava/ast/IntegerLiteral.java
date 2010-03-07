package minijava.ast;

import minijava.visitor.Visitor;

public class IntegerLiteral extends Expression {

	public final int value;

	public IntegerLiteral(int value) {
		super();
		super.setType(IntegerType.instance);
		this.value = value;
	}

	public IntegerLiteral(String image) {
		this(Integer.parseInt(image));
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
