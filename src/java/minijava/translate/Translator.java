package minijava.translate;

import java.io.File;

import minijava.ir.frame.Frame;
import minijava.ir.temp.Label;
//import minijava.translate.implementation.TranslateImplementation;
import minijava.typechecker.MiniJavaTypeChecker;
import minijava.typechecker.TypeChecked;
import minijava.typechecker.TypeCheckerException;

/**
 * This is the interface for the translation to IR phase. To avoid making this
 * specific to how previous phases get called exactly, we expect this interface to
 * accept Strings or input files (rather than someting more structured like an AST) 
 * and call the preceding phases in an appropriate manner
 * to get the files parsed and typechecked before attempting IR generation.
 * <p>
 * As always, you are free to change this code in any way you like as long as
 * it doesn't break the provided unit tests.
 * 
 * @author kdvolder
 */
public class Translator {
/*
	public static Fragments translate(Frame frameFactory, String program) throws TypeCheckerException, Exception {
		TypeChecked typechecked = MiniJavaTypeChecker.parseAndCheck(program);
		return new TranslateImplementation(frameFactory, typechecked).translate();
	}

	public static Fragments translate(Frame frameFactory, File program) throws TypeCheckerException, Exception {
		TypeChecked typechecked = MiniJavaTypeChecker.parseAndCheck(program);
		return new TranslateImplementation(frameFactory, typechecked).translate();
	}
*/
	/**
	 * The translator should use this label for the MiniJava main method.
	 */
	public static final Label L_PRINT = Label.get("mj_println");
	
	/*
	 * The labels below are supposed to point to "special" procedures
	 * that are part of the minijava runtime library.
	 * <p>
	 * The translator assumes that they are magically defined somehow.
	 */
	public static final Label L_MAIN = Label.get("mj_main");
	public static final Label L_NEW_OBJECT = Label.get("mj_new_object");
	public static final Label L_NEW_ARRAY = Label.get("mj_new_array");

}
