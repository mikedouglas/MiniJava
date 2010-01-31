package minijava.typechecker.implementation;

import minijava.ast.Program;
import minijava.typechecker.TypeChecked;
import minijava.util.FunTable;
import minijava.util.ImpTable;
import minijava.visitor.ClassBuilderVisitor;
import minijava.visitor.TypeCheckerVisitor;

public class TypeCheckerImplementation {
	
	Program program;
	ImpTable<ClassEntry> classTable;
	
	public TypeCheckerImplementation(Program program) {
		this.program = program;
	}

	public TypeChecked typeCheck() {
		ClassBuilderVisitor builder = new ClassBuilderVisitor();
		classTable = builder.visit(program);
		TypeCheckerVisitor checker = new TypeCheckerVisitor(classTable);
		return null;
	}

	public Object buildClassTable() {
		ClassBuilderVisitor builder = new ClassBuilderVisitor();
		classTable = builder.visit(program);
		return classTable;
	}

}
