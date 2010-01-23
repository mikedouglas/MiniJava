package minijava.ast;

import minijava.visitor.Visitor;

public class If extends Statement {
	
	public final Expression tst;
	public final Statement thn;
	public final Statement els;
	
	public If(Expression tst, Statement thn, Statement els) {
		super();
		this.tst = tst;
		this.thn = thn;
		this.els = els;
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}
	
}
