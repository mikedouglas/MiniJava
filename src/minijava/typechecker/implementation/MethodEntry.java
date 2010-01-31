package minijava.typechecker.implementation;

import java.util.Map.Entry;

import minijava.ast.Type;
import minijava.util.ImpTable;
import minijava.util.Indentable;
import minijava.util.IndentingWriter;

public class MethodEntry implements Indentable {
	ImpTable<Type> variables;
	ImpTable<Type> parameters;
	
	public MethodEntry() {
		variables = new ImpTable<Type>();
		parameters = new ImpTable<Type>();
	}
	
	public ImpTable<Type> getVariables() {
		return variables;
	}
	
	public ImpTable<Type> getParameters() {
		return parameters;
	}
	
	public void setVariables(ImpTable<Type> variables) {
		this.variables = variables;
	}
	
	public void setParameters(ImpTable<Type> parameters) {
		this.parameters = parameters;
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
