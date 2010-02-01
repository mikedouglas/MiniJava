package minijava.typechecker.implementation;

import java.util.Map.Entry;

import minijava.ast.Type;
import minijava.util.ImpTable;
import minijava.util.Indentable;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class ClassEntry implements Indentable {
	
	String parentName;
	ClassEntry parentClass;
	ImpTable<MethodEntry> methods;
	ImpTable<Type> fields;
	final String className;
	
	public String getClassName() {
		return className;
	}
	
	public ClassEntry (String className, String parentName) {
		methods = new ImpTable<MethodEntry>();
		fields = new ImpTable<Type>();
		this.className = className;
		this.parentName = parentName;
		this.parentClass = null;
	}

	public ImpTable<MethodEntry> getMethods() {
		return methods;
	}

	public void setMethods(ImpTable<MethodEntry> methods) {
		this.methods = methods;
	}

	public ImpTable<Type> getFields() {
		return fields;
	}

	public void setFields(ImpTable<Type> fields) {
		this.fields = fields;
	}
	
	public String getParentName() {
		return parentName;
	}
	
	public ClassEntry getParentClass() {
		return parentClass;
	}
	
	public void setParentClass(ClassEntry parentClass) {
		this.parentClass = parentClass;
	}
	
	public Type lookupVariable(String id) {
		Type field = fields.lookup(id);
		
		if (field == null && parentClass != null) {
			field = parentClass.lookupVariable(id);
		}
		
		return field;
	}

	public MethodEntry lookupMethod(String id) {
		MethodEntry method = methods.lookup(id);
		
		if (method == null && parentClass != null) {
			method = parentClass.lookupMethod(id);
		}
		
		return method;
	}
	
	@Override
	public void dump(IndentingWriter out) {
		out.println();
		out.indent();
		
		out.println("Methods:");
		out.indent();
		for (Entry<String, MethodEntry> entry : methods) {
			out.print(entry.getKey());
			out.print(": ");
			out.print(entry.getValue());
		}
		out.outdent();
		
		out.println("Fields:");
		out.indent();
		for (Entry<String, Type> entry : fields) {
			out.print(entry.getKey());
			out.print(": ");
			out.print(entry.getValue());
			out.println();
		}
		out.outdent();
		out.outdent();
	}
}
