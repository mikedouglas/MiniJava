package minijava.visitor;

import minijava.ast.AST;
import minijava.ast.And;
import minijava.ast.ArrayAssign;
import minijava.ast.ArrayLength;
import minijava.ast.ArrayLookup;
import minijava.ast.Assign;
import minijava.ast.BooleanLiteral;
import minijava.ast.BooleanType;
import minijava.ast.Call;
import minijava.ast.ClassDecl;
import minijava.ast.Expression;
import minijava.ast.IdentifierExp;
import minijava.ast.If;
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
import minijava.ast.Print;
import minijava.ast.Program;
import minijava.ast.This;
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
		ClassEntry classEntry = classTable.lookup(n.className);
		if(classEntry == null)
			reporter.undefinedId(n.className);
		
		MethodEntry mainMethod = classEntry.lookupMethod("main");
		
		visit(n.statement,mainMethod);
	}

	public void visit(ClassDecl n) {
		ClassEntry classEntry = classTable.lookup(n.name);

		if(classEntry == null)
			reporter.undefinedId(n.name);

		visit(n.vars, classEntry);
		visit(n.methods, classEntry);
	}

	public void visit(MethodDecl n, ClassEntry entry) {	
		MethodEntry method = entry.getMethods().lookup(n.name);
		
		visit(n.vars, method);
		visit(n.statements, method);
		
		//must check return type after local vars to pass unit tests.
		
		visit(method.getReturnType());
		Type returnedType = (Type) visit(n.returnExp, method);

		if (returnedType!=null)
		{
			// check here if the return type even exists
			visit(returnedType);
			if (! returnedType.equals(method.getReturnType())) {
				reporter.typeError(n.returnExp, method.getReturnType(), returnedType);
			}
		} else {
			reporter.typeError(n.returnExp, method.getReturnType(), returnedType);		
		}

		for(Type t:method.getParamTypes())
			visit(t);
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
		//also need to check the params are the right types
		
		Type reciever = (Type) visit(exp.receiver,method);//this should resolve to an id (do we allow static methods?)
		
		if (reciever == null || reciever.getClass()!=ObjectType.class) {
			reporter.typeErrorExpectObjectType(exp, reciever);
		}else{
			//check that this object actually has a corresponding method
			String className = ((ObjectType)reciever).name;
			ClassEntry classEntry = classTable.lookup(className);
			if(classEntry == null)
				reporter.undefinedId(className);
			else{
				MethodEntry recMethod = classEntry.lookupMethod(exp.name);
				if(recMethod==null)
					reporter.undefinedId(exp.name);
				else{
					
					
					NodeList<Expression> args = exp.rands;
					for(int i = 0;i<Math.min(args.size(), method.getParamTypes().size());i++)
					{
						Type expectedType = recMethod.getParamTypes().get(i);
						Expression e = args.elementAt(i);
						Type argType = (Type) visit(e,method);
						visit(argType);
						if(argType==null || ! argType.equals(expectedType))
						{
							reporter.typeError(e, expectedType, argType);
						}
					}
					
					if(args.size()!=recMethod.getParamTypes().size())
						reporter.wrongNumberOfArguments(exp, method.getParamTypes().size());
					
					
					return recMethod.getReturnType();
				}
			}
		}
	
		return null;
		
	}
	
	public void visit(If exp, MethodEntry method) {
		Type actualType = (Type) visit(exp.tst, method);
		
		if (actualType == null || !actualType.equals(BooleanType.instance)) {
			reporter.typeError(exp.tst, BooleanType.instance, actualType);
		}
		
		visit(exp.els, method);
		visit(exp.thn, method);
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
	
	/**
	 * This visitor is for instance field declarations
	 * @param exp
	 * @param method
	 * @return
	 */
	public Type visit(VarDecl exp, ClassEntry method) {
		
		visit(exp.type);

		return exp.type;
	}
	
	public Type visit(VarDecl exp, MethodEntry method) {
		
		visit(exp.type);

		return exp.type;
	}
	
	public Type visit(ArrayAssign exp, MethodEntry method) {
		Type arrayType = method.lookupVariable(exp.name);
		
		if(arrayType==null)
		{
			reporter.undefinedId(exp.name);
		}else if (! arrayType.equals(IntArrayType.instance))
		{
			//create an expression to pass to the error report.
			reporter.typeError(new IdentifierExp(exp.name), IntArrayType.instance, arrayType);
		}
		
		
		Type type = (Type) visit(exp.index,method);
		if(type==null|| ! type.equals(IntegerType.instance))
		{
			reporter.typeError(exp.index, IntegerType.instance, type);
		}
		
		Type valType = (Type) visit(exp.value,method);
		if(valType==null|| ! valType.equals(IntegerType.instance))
		{
			reporter.typeError(exp.value, IntegerType.instance, valType);
		}
		
		return IntegerType.instance;
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
				 visit(assign.value, method);
			}
			else if (! expectedType.equals(actualType)) {
				reporter.typeError(assign.value, expectedType, actualType);
			}
		}
		//return expectedType; //Do assignments return values in minijava?
	}
	
	public void visit(Print exp, MethodEntry method) {
		//apparently, print is only allowed to print integer variables, not booleans
		Type type = (Type) visit(exp.exp,method);
		if(type == null || ! type.equals(IntegerType.instance))
			reporter.typeError(exp.exp, IntegerType.instance, type);
	}
	
	public void visit(ObjectType type) {
		if (classTable.lookup(type.name) == null) {
			reporter.undefinedId(type.name);
		}
	}
	
	public void visit(IntegerType type) {

	}
	
	public void visit(BooleanType type) {

	}
	
	public void visit(IntArrayType type) {

	}
	
	public Type visit(This t, MethodEntry method) {
		return new ObjectType(method.getParent().getClassName());
	}
	
	public Type visit(IntegerLiteral lit) {
		return new IntegerType();
	}
	
	public Type visit(BooleanLiteral lit) {
		return new BooleanType();
	}
	
	
}
