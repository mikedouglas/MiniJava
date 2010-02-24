package minijava.typechecker;


/**
 * A specific Exception class for our type checker. See the class ErrorReport
 * to see how to create and raise TypeCheckerExceptions
 */
public class TypeCheckerException extends Exception {
	
	private static final long serialVersionUID = 1965921755523813030L;
	
	private ErrorMessage firstError;

	TypeCheckerException(ErrorMessage firstError) {
		this.firstError = firstError;
	}

	public ErrorMessage getFirstMessage() {
		return firstError;
	}
	
	@Override
	public String toString() {
		return "TypeCheckerException("+firstError+")";
	}
}
