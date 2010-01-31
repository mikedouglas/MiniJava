package minijava.visitor;

import minijava.ast.AST;
import minijava.ast.And;
import minijava.ast.ArrayLength;
import minijava.ast.ArrayLookup;
import minijava.ast.Assign;
import minijava.ast.BooleanLiteral;
import minijava.ast.BooleanType;
import minijava.ast.Call;
import minijava.ast.ClassDecl;
import minijava.ast.Expression;
import minijava.ast.IdentifierExp;
import minijava.ast.IntArrayType;
import minijava.ast.IntegerLiteral;
import minijava.ast.IntegerType;
import minijava.ast.LessThan;
import minijava.ast.MainClass;
import minijava.ast.MethodDecl;
import minijava.ast.Minus;
import minijava.ast.NewArray;
import minijava.ast.NewObject;
import minijava.ast.NodeList;
import minijava.ast.Not;
import minijava.ast.ObjectType;
import minijava.ast.Plus;
import minijava.ast.Program;
import minijava.ast.Times;
import minijava.ast.Type;
import minijava.ast.VarDecl;
import minijava.typechecker.ErrorReport;
import minijava.typechecker.implementation.ClassEntry;
import minijava.typechecker.implementation.MethodEntry;
import minijava.util.ImpTable;

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
		
		visit(n.vars);
		visit(n.methods, classEntry);
	}

	public void visit(MethodDecl n, ClassEntry entry) {	
		MethodEntry method = entry.getMethods().lookup(n.name);
		
		Type returnedType = (Type) visit(n.returnExp, method);
		
		if (returnedType != null) {
			visit(returnedType);
		
			if (! returnedType.equals(method.getReturnType())) {
				reporter.typeError(n.returnExp, method.getReturnType(), returnedType);
			}
		}
		
		visit(n.vars, method);
		visit(n.statements, method);
	}
	
	public void visit(VarDecl var) {
		visit(var.type);
	}
	
	public Type visit(IdentifierExp idExp, MethodEntry method) {
		Type type = method.lookupVariable(idExp.name);
		
		if (type == null) {
			reporter.undefinedId(idExp.name);
		}
		
		return type;
	}

	
	public Type visit(IntegerLiteral exp, MethodEntry method) {
		return IntegerType.instance;
	}
	
	public Type visit(BooleanLiteral exp, MethodEntry method) {
		return BooleanType.instance;
	}
	
	public Type visit(Call exp, MethodEntry method) {
		
		Type type = method.lookupMethod(exp.name);
		
		if (type == null) {
			reporter.undefinedId(exp.name);
		}
		
		return type;
		
	}
	
	public Type visit(ArrayLength exp, MethodEntry method) {
		//array lengths are always integers. 
		//So what we actually have to do here is check that the array expression contains an array
		
		Type type = (Type) visit(exp.array,method);
		if (type==null || ! type.equals(IntArrayType.instance))
		{
			reporter.typeError(exp.array, IntArrayType.instance, type);
		}
		
		return IntegerType.instance;
	}
	
	public Type visit(Minus exp, MethodEntry method) {		
		checkMathBinop(exp.e1,exp.e2,method);	
		return IntegerType.instance;
	}
	public Type visit(Plus exp, MethodEntry method) {		
		checkMathBinop(exp.e1,exp.e2,method);	
		return IntegerType.instance;
	}
	public Type visit(Times exp, MethodEntry method) {		
		checkMathBinop(exp.e1,exp.e2,method);	
		return IntegerType.instance;
	}
	public Type visit(LessThan exp, MethodEntry method) {		
		checkMathBinop(exp.e1,exp.e2,method);	
		return BooleanType.instance;
	}
	
	public Type visit(And exp, MethodEntry method) {		
		checkBooleanBinop(exp.e1,exp.e2,method);	
		return BooleanType.instance;
	}
	
	public Type visit(Not exp, MethodEntry method) {		
		Type type = (Type) visit(exp.e,method);
		if(type==null|| ! type.equals(BooleanType.instance))
		{
			reporter.typeError(exp.e, BooleanType.instance, type);
		}
		return BooleanType.instance;
	}
	
	
	private void checkMathBinop(Expression left, Expression right, MethodEntry method) {
		Type typeLeft = (Type) visit(left,method);
		Type typeRight = (Type) visit(right,method);
		
		if(typeLeft==null|| ! typeLeft.equals(IntegerType.instance))
		{
			reporter.typeError(left, IntegerType.instance, typeLeft);
		}else if(typeRight==null|| ! typeRight.equals(IntegerType.instance))
		{
			reporter.typeError(right, IntegerType.instance, typeRight);
		}
	}
	
	
	private void checkBooleanBinop(Expression left, Expression right, MethodEntry method) {
		Type typeLeft = (Type) visit(left,method);
		Type typeRight = (Type) visit(right,method);
		
		if(typeLeft==null|| ! typeLeft.equals(BooleanType.instance))
		{
			reporter.typeError(left, BooleanType.instance, typeLeft);
		}else if(typeRight==null|| ! typeRight.equals(BooleanType.instance))
		{
			reporter.typeError(right, BooleanType.instance, typeRight);
		}
	}

	public Type visit(NewArray exp, MethodEntry method) {
		
		Type type = (Type) visit(exp.size,method);
		if(type==null|| ! type.equals(IntegerType.instance))
		{
			reporter.typeError(exp.size, IntegerType.instance, type);
		}
		
		return IntArrayType.instance;
	}
	

	public Type visit(NewObject exp, MethodEntry method) {
		
		ClassEntry classEntry = classTable.lookup(exp.typeName);
		if(classEntry == null)
		{
			reporter.undefinedId(exp.typeName);
		}
		return new ObjectType(exp.typeName);
	}

	public Type visit(ArrayLookup exp, MethodEntry method) {
		
		Type arrayType = (Type) visit(exp.array,method);
		if(arrayType==null|| ! arrayType.equals(IntArrayType.instance))
		{
			reporter.typeError(exp.index, IntegerType.instance, arrayType);
		}
		
		
		Type type = (Type) visit(exp.index,method);
		if(type==null|| ! type.equals(IntegerType.instance))
		{
			reporter.typeError(exp.index, IntegerType.instance, type);
		}
		
		return IntegerType.instance;
	}


	public void visit(Assign assign, MethodEntry method) {
		Type expectedType = method.lookupVariable(assign.name);
		Type actualType = (Type) visit(assign.value, method);
		
		if (expectedType == null) {
			reporter.undefinedId(assign.name);

		}else{
			if(actualType==null)
			{
				reporter.typeError(assign.value, expectedType, actualType);//null is not correctly caught by the .equals method
			}
			else if (! expectedType.equals(actualType)) {
				reporter.typeError(assign.value, expectedType, actualType);
			}
		}
		//return expectedType; //Do assignments return values in minijava?
	}
	
	public void visit(ObjectType type) {
		if (classTable.lookup(type.name) == null) {
			reporter.undefinedId(type.name);
		}
	}
	
	public Type visit(IntegerLiteral lit) {
		return new IntegerType();
	}
	
	public Type visit(BooleanLiteral lit) {
		return new BooleanType();
	}
	
}
