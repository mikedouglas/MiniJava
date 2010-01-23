package minijava.parser;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import minijava.ast.AST;
import minijava.ast.Program;
import minijava.parser.jcc.JCCMiniJavaParser;
import minijava.parser.jcc.ParseException;

/**
 * Instead of calling methods in the JavaCC (or maybe SableCC) generated
 * parser directly. We use this class as a kind of Stub class to isolate us from
 * direct dependency on the generated parser.
 * <p>
 * In theory this means you should be able to replace the parser
 * with another implementation by editing these methods, and without
 * changing the main implementation for the rest of the compiler.
 * <p>
 * It also has the benefit of not mixing in utility methods with
 * the generated code. We can put different methods for calling the parser
 * (with a file, an inputstream, a String ect. in here).
 * <p>
 * Note: Actually, there is a dependency on the ParseException class generated
 * by JavaCC. To really get "plugability" we should not have this dependency.
 *
 * @author kdvolder
 */
public class MiniJavaParser {

	/**
	 * Read input from a File and parse it into an AST representation.
	 */
	public static Program parse(File file) throws IOException, ParseException {
		FileReader input = new FileReader(file);
		try {
			return parse(input);
		}
		finally { //No matter what happens, always close the file!
			input.close();
		}
	}

	/**
	 * Read input from a java.io.Reader and parse it into an AST. It is the
	 * caller's responsibility to close the Reader.
	 */
	private static Program parse(Reader input) throws ParseException {
		JCCMiniJavaParser parser = new JCCMiniJavaParser(input);
		return parser.Program();
	}

	/**
	 * Read input directly from a String and parse it into an AST.
	 */
	public static Program parse(String inputText) throws ParseException {
		return parse(new StringReader(inputText));
	}

	/**
	 * Pretty print an AST node and return the result as a String.
	 */
	public static String unparse(AST node) {
		return node.toString(); 
		// This assumes that toString on AST nodes is appropriately implemented.
		// If you decided to generate/implement your own AST classes rather than use 
		// the ones we provided for you, you may need to implement this unparse method 
		// differently.
	}
}
