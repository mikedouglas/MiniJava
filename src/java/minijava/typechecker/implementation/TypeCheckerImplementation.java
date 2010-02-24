package minijava.typechecker.implementation;

import java.util.Iterator;
import java.util.Map.Entry;

import minijava.ast.Program;
import minijava.typechecker.ErrorReport;
import minijava.typechecker.TypeChecked;
import minijava.typechecker.TypeCheckerException;
import minijava.util.ImpTable;
import minijava.visitor.ClassBuilderVisitor;
import minijava.visitor.TypeCheckerVisitor;

public class TypeCheckerImplementation {
	
	Program program;
	ImpTable<ClassEntry> classTable;
	ErrorReport reporter;
	public TypeCheckerImplementation(Program program) {
		this.program = program;
	}


	public TypeChecked typeCheck() throws TypeCheckerException{
		reporter = new ErrorReport();
		buildClassTable();
		
		TypeCheckerVisitor checker = new TypeCheckerVisitor(classTable, reporter);
		checker.visit(program);
		
		reporter.close();//this will throw the first exception that was reported, if it exists.
		return null;
	}

	public Object buildClassTable(){
		ClassBuilderVisitor builder = new ClassBuilderVisitor(reporter);
		classTable = builder.visit(program);
		
		// Resolve class inheritance
		Iterator<Entry<String, ClassEntry>> itr = classTable.iterator();
		while (itr.hasNext()) {
			Entry<String, ClassEntry> entry;
			entry = (Entry<String, ClassEntry>) itr.next();
			String parentClassName = entry.getValue().getParentName(); 
			
			if (parentClassName != null) {
				ClassEntry parentClass = classTable.lookup(parentClassName);
				entry.getValue().setParentClass(parentClass);
			}
		}
		
		return classTable;
	}

}
