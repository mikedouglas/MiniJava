package minijava.ast;

import minijava.visitor.Visitor;

public class BooleanType extends Type {
	public static BooleanType instance = new BooleanType();
	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

	@Override
	public boolean equals(Object other) {
		return this.getClass()==other.getClass();
	}
	
}
