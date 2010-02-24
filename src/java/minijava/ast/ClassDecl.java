package minijava.ast;

import java.util.List;

import minijava.visitor.Visitor;

public class ClassDecl extends AST {
	
	public final String name;
	public final String superName; //May be null!
	
	public final NodeList<VarDecl> vars;
	public final NodeList<MethodDecl> methods;
	
	public ClassDecl(String name, String superName, NodeList<VarDecl> vars,
			NodeList<MethodDecl> methods) {
		super();
		this.name = name;
		this.superName = superName;
		this.vars = vars;
		this.methods = methods;
	}

	public ClassDecl(String name, String superName, List<VarDecl> vars,
			List<MethodDecl> methods) {
		this(
			name, superName, 
			new NodeList<VarDecl>(vars), new NodeList<MethodDecl>(methods)
		);
	}

	@Override
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
