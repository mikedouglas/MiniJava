package minijava.test.parser;

import java.io.File;

import minijava.parser.MiniJavaParser;
import minijava.test.SampleCode;

import org.junit.Test;

/**
 * The tests in this class correspond more or less to the work in Chapter 3.
 * <p>
 * These tests try to call your parser to parse MiniJava programs, but they do 
 * not check the AST produced by the parser. As such you should be able to get
 * these tests to pass without inserting any semantic actions into your
 * parser specification file.
 * <p>
 * The tests in this file are written in an order that can be followed if
 * you want to develop your parser incrementally, staring with the tests at the
 * top of the file, and working your way down.
 * 
 * @author kdvolder
 */
public class Test3Parse {
	
	/**
	 * All testing is supposed to go through calling this method to one of the
	 * accept methods, to see whether some input is accepted by the parser. The subclass 
	 * Test4Parse refines these tests by overriding this accept method to also verify the 
	 * parse tree structure.
	 */
	protected void accept(String input) throws Exception {
		MiniJavaParser.parse(input);
	}
	
	protected void accept(File file) throws Exception {
		System.out.println("parsing file: "+file);
		MiniJavaParser.parse(file);
	}
	
	//////////////////////////////////////////////////////////////////////////
	// First let's ensure we can parse the "simplest possible" program:

	@Test
	public void testEmptyMain() throws Exception {
		//Note that if we follow the spec of MiniJava to the letter, then we are not
		//allowed to have an empty main. It must always contain precisely one Statement.
		//To make an "empty" main, we must put a {} inside of the main's body.
		accept( "class SomeName { \n" +
				"   public static void main(String[] args) {\n" +
				"       {}\n" +
				"   }\n" +
				"}");
	}
	
	//////////////////////////////////////////////////////////////////////////
	// Now the "simplest possible program with an expression".
	
	@Test
	public void testPrintMain() throws Exception {
		accept( "class SomeName { \n" +
				"   public static void main(String[] args) {\n" +
				"       System.out.println(0);\n"+
				"   }\n" +
				"}");
	}

	//////////////////////////////////////////////////////////////////////////
	// Next: let's work on making the expression parsing complete.
	
	// Note: some of the created test programs here are not correct. They use undeclared variables
	// and may have type errors... but they should pass by the parser. The parser doesn't
	// check for these kinds of errors.
	
	void acceptExpression(String exp) throws Exception {
		accept( "class SomeName { \n " +
				"   public static void main(String[] args) {\n" +
				"       System.out.println(" + exp + ");\n"+
				"   }\n" +
				"}");
	}
	
	@Test
	public void testBooleanLiteral() throws Exception {
		acceptExpression("true");
		acceptExpression("false");
	}
	
	@Test
	public void testIdentifier() throws Exception {
		acceptExpression("x");
		acceptExpression("y");
		acceptExpression("xy123");
		acceptExpression("x_y_123");
		acceptExpression("x_y_123");
	}
	
	@Test
	public void testThis() throws Exception {
		acceptExpression("this");
	}
	
	@Test
	public void testNot() throws Exception {
		acceptExpression("!true");
		acceptExpression("!!!!!!true");
	}
	
	@Test
	public void testNew() throws Exception {
		acceptExpression("new SomeClassName()");
		acceptExpression("new int[10]");
	}
	@Test
	public void testArray() throws Exception {
		acceptExpression("someArray[10]");
	}
	@Test
	public void testDotExpressions() throws Exception {
		acceptExpression("someArray.length");
		acceptExpression("someObject.doSomeThing()");
		acceptExpression("someObject.doSomeThing(1)");
		acceptExpression("someObject.doSomeThing(1, 2)");
		acceptExpression("someObject.doSomeThing(1, 2, 3, 4, 5)");
		acceptExpression("someObject.doSomeThing(this.getInfo(), this.getMoreInfo())");
		acceptExpression("new SomeClass().runIt()");
		acceptExpression("someObject.flub().flub().flub()");
	}
	@Test
	public void testParens() throws Exception {
		acceptExpression("(1)");
		acceptExpression("((((((1))))))");
	}
	@Test
	public void testMult() throws Exception {
		acceptExpression("10*9");
		acceptExpression("10*9*8");
		acceptExpression("foo.length*foo.length");
		acceptExpression("foo[x]*foo[y]");
		acceptExpression("foo.getSize()*foo.getFlubs()");
		acceptExpression("10*9*8*7*x*foo.length*foo.getSize()*array[x*2]");
	}
	
	@Test
	public void testAdd() throws Exception {
		acceptExpression("10+9");
		acceptExpression("10-9");
		acceptExpression("10+9+8");
		acceptExpression("10-9-8");
		acceptExpression("foo.length+foo.length");
		acceptExpression("foo.length-foo.length");
		acceptExpression("foo[x]+foo[y]");
		acceptExpression("foo.getSize()+foo.getFlubs()");
		acceptExpression("10+9+x*foo.length-foo.getSize()+array[x+2]");
		acceptExpression("(a-b)*(a+b)");
	}

	@Test
	public void testComp() throws Exception {
		acceptExpression("10<9");
		acceptExpression("10+a*3<9-4+2");
		acceptExpression("foo.length<1");
		acceptExpression("i<foo.getSize()");
	}
	
	@Test
	public void testAnd() throws Exception {
		acceptExpression("a&&b");
		acceptExpression("low<x && x<hi && !(x<0)");
	}
	
	
	/////////////////////////////////////////////////////////////////////////////////
	// Now let's work on making statement parsing complete.
	
	void acceptStatement(String statement) throws Exception {
		accept( "class SomeName { \n " +
				"   public static void main(String[] args) {\n" +
				statement+"\n"+
				"   }\n" +
				"}");
	}
	
	@Test public void testAssign() throws Exception {
		acceptStatement("numbers[x] = numbers[x+1];");
		acceptStatement("foo = foo+1;");
	}
	
	@Test public void testIf() throws Exception {
		acceptStatement("if (x < y) largest = y; else largest = x;");
		acceptStatement(
				"if (x < y) {\n" +
				"   largest = y;\n" +
				"   smallest = x;\n" +
				"} else { " +
				"   largest = x;" +
				"   smallest = y;" +
				"}");
	}
	@Test public void testWhile() throws Exception {
		acceptStatement(
				"while (x < y) {\n" +
				"   System.out.println(x);\n" +
				"   x = x+1;\n" +
				"}"); 
	}
	
	/////////////////////////////////////////////////////////////////////////////////
	// All that remains now is the declaration of classes with methods and fields
	private void acceptClass(String clsDecl) throws Exception {
		accept( "class SomeMainClass { \n " +
				"   public static void main(String[] args) {\n" +
				"      {}\n" +
				"   }\n" +
				"}\n" +
				clsDecl);
	}
	
	@Test public void testEmptyClass() throws Exception {
		acceptClass("class SomeClassName {}");
	}
	
	@Test public void testExtendsClaus() throws Exception {
		acceptClass("class SomeClassName extends SomeSuperClass {}");
	}
	
	@Test public void testVariable() throws Exception {
		acceptClass(
				"class AxisPoint {\n" +
				"  int x;\n" +
				"}");
		acceptClass(
				"class Point {\n" +
				"  int x;\n" +
				"  int y;\n" +
				"}");
		acceptClass(
				"class FlipFlop {\n" +
				"  boolean value;\n" +
				"}");
		acceptClass(
				"class Vector {\n" +
				"  int[] nums;\n" +
				"}");
		acceptClass(
				"class LinkedList {\n" +
				"  int value;\n" +
				"  LinkedList next;\n" +
				"}");
	}
	
	@Test public void testMethod() throws Exception {
		acceptClass(
				"class Box {\n" +
				"    int x;\n" +
				"    public int getX() {\n" +
				"       return x;" +
				"    }" +
				"}");
		acceptClass(
				"class Box {\n" +
				"    public int getX() {\n" +
				"       return x;" +
				"    }" +
				"    int x;\n" +
				"}");
		acceptClass(
				"class Box {\n" +
				"    public int getX() {\n" +
				"       return x;" +
				"    }" +
				"    int x;\n" +
				"    public int get2X() {\n" +
				"       return 2*x;" +
				"    }" +
				"}");
		acceptClass(
				"class Point {\n" +
				"    int x;\n" +
				"    int y;\n" +
				"    public int getX() {\n" +
				"       return x;" +
				"    }" +
				"    public int getY() {\n" +
				"       return y;" +
				"    }" +
				"}");
		acceptClass(
				"class Foo {\n" +
				"    public int params(int i, int[] nums, Foo foo) {\n" +
				"       Bar bar;\n" +
				"       int local;\n" +
				"       local = nums.length;\n" +
				"       if (i < local)\n" +
				"          local = nums[i];\n" +
				"       else" +
				"          local = 0;\n" +
				"       return local;" +
				"    }" +
				"}");
	}
	
	/////////////////////////////////////////////////////////////////////////////////
	// Finally, check whether the parser accepts all the book's sample code.
	@Test 
	public void testParseSampleCode() throws Exception {
		File[] files = SampleCode.sampleFiles();
		for (File file : files) {
			accept(file);
		}
	}
	
}
