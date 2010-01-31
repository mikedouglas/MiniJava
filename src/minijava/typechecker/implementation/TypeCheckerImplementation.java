package minijava.typechecker.implementation;

import minijava.ast.Program;
import minijava.typechecker.TypeChecked;
import minijava.util.FunTable;
import minijava.util.ImpTable;
import minijava.visitor.ClassBuilderVisitor;

public class TypeCheckerImplementation {
	
	Program program;
	ImpTable<ClassEntry> classTable;
	
	public TypeCheckerImplementation(Program program) {
		this.program = program;
	}

	public TypeChecked typeCheck() {
		
		return null;
	}

	public Object buildClassTable() {
		ClassBuilderVisitor builder = new ClassBuilderVisitor();
		classTable = builder.visit(program);
		return classTable;
	}

}
