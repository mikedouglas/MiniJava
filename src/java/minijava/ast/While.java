package minijava.ast;

import minijava.visitor.Visitor;


public class While extends Statement {

	public final Expression tst;
	public final Statement body;
	
	public While(Expression tst, Statement body) {
		super();
		this.tst = tst;
		this.body = body;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
