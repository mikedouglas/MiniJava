package minijava.typechecker;

import java.io.File;

import minijava.ast.Program;
import minijava.parser.MiniJavaParser;
import minijava.typechecker.implementation.TypeCheckerImplementation;

/**
 * This file constitutes the "interface" with the MiniJavaTypeChecker. 
 * It provides a few static methods that allow the MiniJava checker to be
 * invoked on a given input program.
 * <p>
 * You may change the implementations of these methods, or any other methods
 * and classes in this project, as long as your changes are compatible with
 * the unit tests provided in the test packages.
 * 
 * @author kdvolder
 */
public class MiniJavaTypeChecker {
	
	/**
	 * Parse the contents of a given file and typecheck it. If type checking succeeds
	 * the method returns normally. 
	 * <p>
	 * If the program has a type error or undeclared identifier error then an appropriate 
	 * TypeCheckerException must be raised. The type checker may try to continue checking
	 * after the first error is encountered, but should nevertheless still raise a 
	 * TypeCheckerException (this can be done by postponing the raising of the Exception
	 * until all of the input has been processed). See the class {@link ErrorReport}
	 * <p>
	 * Other Exceptions may be raised if the parsing or reading of the file fails.
	 * <p>
	 * The TypeChecker may return some representation of the TypeChecked program along with
	 * useful information derived by the checker.
	 * <p>
	 * This information will be passed along to the next phase of the compiler. 
	 * <p>
	 * Right now what information goes in TypeChecked is irrelevant. All that matters is
	 * that errors get discovered. In later stages of the compiler however, you may find
	 * that your type checker computed valuable information (such as the symbol table from
	 * phase 1, which you may want to use again). At this point you will be able to
	 * add information into the TypeChecked object without breaking the type checker
	 * tests.
	 */
	public static TypeChecked parseAndCheck(File file) throws TypeCheckerException, Exception {
		Program program = MiniJavaParser.parse(file);
		return new TypeCheckerImplementation(program).typeCheck();
	}

	public static TypeChecked parseAndCheck(String input) throws TypeCheckerException, Exception {
		Program program = MiniJavaParser.parse(input);
		return new TypeCheckerImplementation(program).typeCheck();
	}

}
