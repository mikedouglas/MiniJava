package minijava.typechecker.implementation;

import java.util.Map.Entry;

import minijava.ast.Type;
import minijava.util.ImpTable;
import minijava.util.Indentable;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class ClassEntry implements Indentable {
	
	ImpTable<MethodEntry> methods;
	ImpTable<Type> fields;
	final String className;
	
	public String getClassName() {
		return className;
	}

	public ClassEntry (String className) {
		methods = new ImpTable<MethodEntry>();
		fields = new ImpTable<Type>();
		this.className = className;
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
	
	public Type lookupVariable(String id) {
		return fields.lookup(id);
	}

	public MethodEntry lookupMethod(String id) {
		return methods.lookup(id);
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
