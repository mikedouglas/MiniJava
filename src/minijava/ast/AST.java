package minijava.ast;

import java.io.PrintWriter;
import java.io.StringWriter;

import minijava.visitor.PrettyPrintVisitor;
import minijava.visitor.Visitor;

public abstract class AST {
	
	public abstract <R> R accept(Visitor<R> v);
	
	@Override
	public String toString() {
		StringWriter out = new StringWriter();
		this.accept(new PrettyPrintVisitor(new PrintWriter(out)));
		return out.toString();
	}

}
