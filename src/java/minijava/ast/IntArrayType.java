package minijava.ast;

import minijava.visitor.Visitor;

public class IntArrayType extends Type {
	public static IntArrayType instance = new IntArrayType();
	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}
	
	@Override
	public boolean equals(Object other) {
		return this.getClass()==other.getClass();
	}


}
