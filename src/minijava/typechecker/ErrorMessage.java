package minijava.typechecker;

import minijava.ast.Call;
import minijava.ast.Expression;
import minijava.ast.ObjectType;
import minijava.ast.Type;

/**
 * A class to represent Entries in an Error report.
 * It provides static methods to create different types of messages.
 * <p>
 * If we assume that the TypeChecker only uses the ErrorReport and
 * ErrorMessage classes to create error reports than we can use this
 * to write relatively precise unit tests for the expected error output
 * without specifying the exact syntax of expected error messages 
 * (Unit tests can use the instances of ErrorMessage class or assume that
 * the implementation of the ErrorMessage class produces similar error Strings.
 */
public class ErrorMessage {
	
	private String msg;

	/**
	 * Constructor is private so that you can't make messages that are
	 * "arbitrarily formatted".
	 */
	private ErrorMessage(String msg) {
		this.msg = msg;
	}

	public static ErrorMessage undefinedId(String name) {
		return new ErrorMessage("Unbound Identifier: "+name);
	}
	
	public static ErrorMessage duplicateDefinition(String name) {
		return new ErrorMessage("Multiply defined Identifier: "+name);
	}
	
	public static ErrorMessage typeError(Expression exp, Type expected,
			Type actual) {
		return new ErrorMessage(exp+" has type "+actual+" expected "+expected);
	}

	public static ErrorMessage typeErrorExpectObject(Expression exp, Type actual) {
		return new ErrorMessage(exp+" has type "+actual+" expected an object type");
	}
	
	public static ErrorMessage wrongNumberOfArguments(Call call, int expected) {
		return new ErrorMessage("Wrong number of arguments (expected "+expected+"): "+call);
	}
	
	public static ErrorMessage badMethodOverriding(ObjectType objectType,
			String methodName) {
		return new ErrorMessage("Bad overriding in class "+objectType+" for method "+methodName);
	}

	public static ErrorMessage fieldOverriding(ObjectType cls, String fieldName) {
		return new ErrorMessage("Class "+cls+" overrides field "+fieldName);
	}

	@Override
	public String toString() {
		return "ERROR -- "+msg;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((msg == null) ? 0 : msg.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ErrorMessage other = (ErrorMessage) obj;
		if (msg == null) {
			if (other.msg != null)
				return false;
		} else if (!msg.equals(other.msg))
			return false;
		return true;
	}


}
