package minijava.ast;

import java.util.List;

import minijava.visitor.Visitor;

public class MethodDecl extends AST {
	
	public final Type returnType;
	public final String name;
	public final NodeList<VarDecl> formals;
	public final NodeList<VarDecl> vars;
	public final NodeList<Statement> statements;
	public final Expression returnExp;
	
	public MethodDecl(Type returnType, String name, NodeList<VarDecl> formals,
			NodeList<VarDecl> vars, NodeList<Statement> statements, Expression returnExp) {
		super();
		this.returnType = returnType;
		this.name = name;
		this.formals = formals;
		this.vars = vars;
		this.statements = statements;
		this.returnExp = returnExp;
	}

	public MethodDecl(Type returnType, String name, List<VarDecl> formals,
			List<VarDecl> vars, List<Statement> statements, Expression returnExp) {
		this(
			returnType, name, new NodeList<VarDecl>(formals), 
			new NodeList<VarDecl>(vars),
			new NodeList<Statement>(statements),
			returnExp
		);
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
