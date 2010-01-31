package minijava.typechecker.implementation;

import java.util.Map.Entry;

import minijava.ast.Type;
import minijava.util.ImpTable;
import minijava.util.Indentable;
import minijava.util.IndentingWriter;

public class MethodEntry implements Indentable {
	ClassEntry parent; 
	ImpTable<Type> variables;
	ImpTable<Type> parameters;
	
	public MethodEntry(ClassEntry parent) {
		this.parent = parent;
		variables = new ImpTable<Type>();
		parameters = new ImpTable<Type>();
	}
	
	public ImpTable<Type> getVariables() {
		return variables;
	}
	
	public void setVariables(ImpTable<Type> variables) {
		this.variables = variables;
	}
	
	public ImpTable<Type> getParameters() {
		return parameters;
	}
	
	public void setParameters(ImpTable<Type> parameters) {
		this.parameters = parameters;
	}
	
	public Type lookup(String id) {
		Type type = null;
		Type type1 = variables.lookup(id);
		Type type2 = parameters.lookup(id);
		
		if (type1 == type2 && type1 == null) {
			type = parent.lookup(id);
		}
		else if (type1 == null) {
			type = type2;
		}
		else if (type2 == null) {
			type = type1;
		}
		
		return type;
	}
	
	@Override
	public void dump(IndentingWriter out) {		
		out.indent();
		out.println();
		out.println("Parameters: ");
		
		out.indent();
		for (Entry<String, Type> varEntry : parameters) {
			out.print(varEntry.getKey());
			out.print(" :: ");
			out.print(varEntry.getValue());
			out.println();
		}
		out.outdent();
		
		out.println("Variables: ");
		out.indent();
		for (Entry<String, Type> varEntry : variables) {
			out.print(varEntry.getKey());
			out.print(" :: ");
			out.print(varEntry.getValue());
			out.println();
		}
		out.outdent();
		
		out.outdent();
	}
	
}
