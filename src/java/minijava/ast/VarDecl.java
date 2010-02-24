package minijava.ast;

import minijava.visitor.Visitor;


public class VarDecl extends AST {

	public static enum Kind {
		FIELD, LOCAL, FORMAL
	}

	public final Kind   kind;
	public final Type   type;
	public final String name;

	public VarDecl(Kind kind, Type type, String name) {
		super();
		this.kind = kind;
		this.type = type;
		this.name = name;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
