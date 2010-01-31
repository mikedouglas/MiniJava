package minijava.visitor;

import minijava.ast.AST;
import minijava.ast.Assign;
import minijava.ast.ClassDecl;
import minijava.ast.IdentifierExp;
import minijava.ast.MainClass;
import minijava.ast.MethodDecl;
import minijava.ast.NodeList;
import minijava.ast.ObjectType;
import minijava.ast.Program;
import minijava.ast.Type;
import minijava.ast.VarDecl;
import minijava.typechecker.ErrorReport;
import minijava.typechecker.implementation.ClassEntry;
import minijava.typechecker.implementation.MethodEntry;
import minijava.util.ImpTable;
import minijava.util.ImpTable.DuplicateException;

public class TypeCheckerVisitor extends ReflectionVisitor {
	ImpTable<ClassEntry> classTable;
	ErrorReport reporter;
	
	public TypeCheckerVisitor(ImpTable<ClassEntry> classTable, ErrorReport reporter) {
		this.classTable = classTable;
		this.reporter = reporter;
	}
	
	public <T extends AST> void visit(NodeList<T> nodeList) {
		for (int i = 0; i < nodeList.size(); i++) {
			visit(nodeList.elementAt(i));
		}
	}
	
	public <T extends AST> void visit(NodeList<T> nodeList, ClassEntry entry) {
		for (int i = 0; i < nodeList.size(); i++) {
			visit(nodeList.elementAt(i), entry);
		}
	}
	
	public <T extends AST> void visit(NodeList<T> nodeList, MethodEntry entry) {
		for (int i = 0; i < nodeList.size(); i++) {
			visit(nodeList.elementAt(i), entry);
		}
	}

	public void visit(Program n) {
		visit(n.mainClass);
		visit(n.classes);
	}
	
	public void visit(MainClass n) {
		visit(n.statement, classTable.lookup(n.className));
	}

	public void visit(ClassDecl n) {
		ClassEntry classEntry = classTable.lookup(n.name);
		
		visit(n.vars, classEntry);
		visit(n.methods, classEntry);
	}

	public void visit(MethodDecl n, ClassEntry entry) {	
		MethodEntry method = entry.getMethods().lookup(n.name);
		visit(n.vars, method);
		visit(n.statements, method);
	}
	
	public Type visit(IdentifierExp idExp, MethodEntry method) {
		Type type = method.lookup(idExp.name);
		
		if (type == null) {
			reporter.undefinedId(idExp.name);
		}
		
		return type;
	}

	
	public void visit(Assign assign, MethodEntry method) {
		Type expectedType = method.lookup(assign.name);
		Type actualType = (Type) visit(assign.value, method);
		
		if (expectedType == null) {
			reporter.undefinedId(assign.name);
		}
		if(actualType==null)
			System.out.println("no type");
		if (! expectedType.equals(actualType)) {
			reporter.typeError(assign.value, expectedType, actualType);
		}
	}
}
