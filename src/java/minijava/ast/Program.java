package minijava.ast;

import java.util.List;

import minijava.visitor.Visitor;

public class Program extends AST {
	
	public final MainClass mainClass;
	public final NodeList<ClassDecl> classes;

	public Program(MainClass mainClass, NodeList<ClassDecl> otherClasses) {
		this.mainClass=mainClass;
		this.classes=otherClasses; 
	}

	public Program(MainClass m, List<ClassDecl> cs) {
		this(m, new NodeList<ClassDecl>(cs));
	}
	
	public <R> R accept(Visitor<R> v) {
		return v.visit(this);
	}

}
