package minijava.test.typechecker;

import java.io.File;

import junit.framework.Assert;

import org.junit.Test;

import minijava.ast.BooleanType;
import minijava.ast.Call;
import minijava.ast.IntArrayType;
import minijava.ast.IntegerType;
import minijava.ast.ObjectType;
import minijava.ast.Type;
import static minijava.parser.MiniJavaParser.*;
import minijava.parser.jcc.ParseException;
import minijava.test.SampleCode;
import minijava.typechecker.ErrorMessage;
import minijava.typechecker.MiniJavaTypeChecker;
import minijava.typechecker.TypeCheckerException;

/**
 * The difficulty in writing tests for this unit of work is that we should,
 * if at all possible try to not make the testing code be dependant on the
 * MiniJava checker returning specific error messages.
 * <p>
 * To try to still have reasonably specific tests that specify relatively
 * precisely what type of error a specific program ought to raise we will:
 * <ul>
 *   <li>Provide you with a class ErrorReport that you should use to create
 *       error reports.
 *   <li>Tests will only inspect the first error in the report.
 *   <li>Tests will be written to avoid ambiguities into what is the "first"
 *       error as much as possible.
 * </ul>
 * 
 * @author kdvolder
 */
public class TypeCheckTest {

	//////////////////////////////////////////////////////////////////////////////////////////
	// Preliminary check....
	
	/**
	 * This test parses and typechecks all the book sample programs. These should
	 * type check without any errors.
	 * <p>
	 * By itself this is not a very good test. E.g. an implementation which does nothing
	 * at all will already pass the test!
	 */
	@Test
	public void testSampleCode() throws Exception {
		File[] sampleFiles = SampleCode.sampleFiles();
		for (int i = 0; i < sampleFiles.length; i++) {
			System.out.println("parsing: "+sampleFiles[i]);
			accept(sampleFiles[i]);
		}
	}

	/////////////////////////////////////////////////////////////////////////////////////////
	// We'll start with checking if the compiler detects duplicate
	// identifier definitions within the same scope.
	
	@Test
	public void duplicateClassName() throws Exception {
		// Duplicate with the Main class name
		expect( ErrorMessage.duplicateDefinition("Main"),
			emptyMain +
			"class Main {}");
		
		// Duplicate with another class name
		expect( ErrorMessage.duplicateDefinition("Other"),
			emptyMain +
			"class Other {}\n" +
			"class Other {}");
	}

	@Test
	public void duplicateFields() throws Exception {
		//If names are diff it should be ok:
		accept(emptyMain+
				"class SomeClass {\n" +
				"   int x;\n" +
				"   int y;\n" +
				"   int z;\n" +
				"}");
		//Same names in diff classes should be ok
		accept(emptyMain+
				"class SomeClass {\n" +
				"   int x;\n" +
				"}\n" +
				"class OtherClass {\n" +
				"   int x;\n" +
				"}\n" +
				"");
		//Same names in same class = bad
		expect( ErrorMessage.duplicateDefinition("same"),
			emptyMain+
			"class SomeClass {\n" +
			"   int same;\n" +
			"   int same;\n" +
			"}");
		//Same names in same class = bad even if not consecutive locations.
		expect( ErrorMessage.duplicateDefinition("same"),
				emptyMain+
				"class SomeClass {\n" +
				"   int same;\n" +
				"   int diff;\n" +
				"   int same;\n" +
				"}");
	}

	@Test
	public void duplicateMethods() throws Exception {
		accept( emptyMain +
				"class SomeClass {\n" +
				"   int same;\n" +
				"   public int same() { return 0; }\n" +
				"}");
		expect( ErrorMessage.duplicateDefinition("same"),
				emptyMain +
				"class SomeClass {\n" +
				"   public int same() { return 1; }\n" +
				"   public int same() { return 0; }\n" +
				"}");
		expect( ErrorMessage.duplicateDefinition("same"),
				emptyMain +
				"class SomeClass {\n" +
				"   public int same() { return 1; }\n" +
				"   public int diff() { return 1; }\n" +
				"   public int same() { return 0; }\n" +
				"}");
		//Overloading not supported so this is an error:
		expect( ErrorMessage.duplicateDefinition("same"),
				emptyMain +
				"class SomeClass {\n" +
				"   public int same() { return 1; }\n" +
				"   public int same(int x) { return x; }\n" +
				"}");
	}
	
	@Test public void sameMethodAndLocal() throws Exception {
		//Methods fields and locals are in different name spaces!
		accept( emptyMain +
				"class SomeClass {\n" +
				"   int foo;\n" +
				"   public int foo(int foo) { return foo; }\n" +
				"}");
	}
	
	@Test
	public void localSameAsParam() throws Exception {
		//Same name for field and param or local is ok
		accept( emptyMain +
				"class SomeClass {\n" +
				"   int x;\n"+
				"   int y;\n"+
				"   public int foo(int x) { int y; return x; }\n" +
				"}");
		expect( ErrorMessage.duplicateDefinition("same"),
				emptyMain +
				"class SomeClass {\n" +
				"   public int foo(int same) { int same; return same; }\n" +
				"}");
	}
	
	///////////////////////////////////////////////////////////////////////////////
	// Checking phase 2 
	
	// 
	// Undefined Types used in various places
	//
	
	@Test
	public void badFieldType() throws Exception {
		accept(	emptyMain+
				"class Foo {\n" +
				"   int i; boolean b; int[] array;\n" +
				"   Foo foo;\n" +
				"   Bar bar;\n" +
				"}\n" +
				"class Bar {\n" +
				"   Foo foo;\n" +
				"   Bar bar;\n" +
				"}");

		//Try some programs with fields of an unknown class type, in different positions
		//in the class decl.
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   Huh f;\n" +
				"}");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   int x;\n" +
				"   boolean b;\n"+
				"   int[] xs;\n"+
				"   Huh f;\n" +
				"}");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   int x;\n" +
				"   Huh f;\n" +
				"   boolean b;\n"+
				"   int[] xs;\n"+
				"}");
	}
	
	@Test
	public void badReturnType() throws Exception {
		accept(	emptyMain+
				"class Foo {\n" +
				"   Bar bar;\n" +
				"   public Bar getBar() { return bar; }\n" +
				"}\n" +
				"class Bar {\n" +
				"   Foo foo;\n" +
				"   public Foo getFoo() { return foo; }\n" +
				"}");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   Foo foo;\n" +
				"   public Foo getFoo() { return foo; }\n" +
				"   public Huh getHuh() { return foo; }\n" +
				"   public int getZero() { return 0; }\n" +
				"}");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   Foo foo;\n" +
				"   public Huh getHuh() { return foo; }\n" +
				"   public Foo getFoo() { return foo; }\n" +
				"   public int getZero() { return 0; }\n" +
				"}");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   Foo foo;\n" +
				"   public Foo getFoo() { return foo; }\n" +
				"   public int getZero() { return 0; }\n" +
				"   public Huh getHuh() { return foo; }\n" +
				"}");
	}
	
	@Test
	public void badParamType() throws Exception {
		accept(	emptyMain+
				"class Foo {\n" +
				"   Foo bar;\n" +
				"   public Foo getFoo(Foo obar) { return obar; }\n" +
				"}\n");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   Foo foo;\n" +
				"   public Foo getFoo() { return foo; }\n" +
				"   public Foo getHuh(Huh huh) { return foo; }\n" +
				"   public int getZero() { return 0; }\n" +
				"}");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   Foo foo;\n" +
				"   public Foo getHuh(Huh huh) { return foo; }\n" +
				"   public Foo getFoo() { return foo; }\n" +
				"   public int getZero() { return 0; }\n" +
				"}");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   Foo foo;\n" +
				"   public Foo getFoo() { return foo; }\n" +
				"   public int getZero() { return 0; }\n" +
				"   public Foo getHuh(Huh huh) { return foo; }\n" +
				"}");
	}
	
	@Test
	public void badLocalType() throws Exception {
		accept(	emptyMain+
				"class Foo {\n" +
				"   Foo bar;\n" +
				"   public Foo getFoo(Foo obar) { Foo lfoo; return obar; }\n" +
				"}\n");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   public Foo getFoo() { Huh huh; int a; int b; return foo; }\n" +
				"}");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   public Foo getFoo() { int a; Huh huh; int b; return foo; }\n" +
				"}");
		expect(ErrorMessage.undefinedId("Huh"), 
				emptyMain+
				"class Foo {\n" +
				"   public Foo getFoo() { int a; int b; Huh huh; return foo; }\n" +
				"}");
	}
	
	@Test public void methodStatementsChecked() throws Exception {
		// See if checker finds errors in statements of a method.
		expect( typeError("false", new IntegerType(), new BooleanType()), 
				emptyMain+
				"class Foo {\n" +
				"   public int test() {\n" +
				"     System.out.println(false); " +
				"     return 0;\n" +
				"   }\n" +
				"}");
	}
	@Test public void methodReturnChecked() throws Exception {
		// See if checker finds errors in the return exp.
		expect( ErrorMessage.undefinedId("huh"),
				emptyMain+
				"class Foo {\n" +
				"   public int test() { return huh; }\n" +
				"}");	
		// See if checker checks the type of return exp
		expect( typeError("false", new IntegerType(), new BooleanType()),
				emptyMain+
				"class Foo {\n" +
				"   public int test() { return false; }\n" +
				"}");	
	}
	
	//
	// Statements
	//
	
	@Test public void badPrint() throws Exception {
		accept(	"class Main {\n" +
				"   public static void main(String[] args) {\n" +
				"      System.out.println(10);\n" +
				"   }\n" +
				"}");
		expect( typeError("true", new IntegerType(), new BooleanType()),
				"class Main {\n" +
				"   public static void main(String[] args) {\n" +
				"      System.out.println(true);\n" +
				"   }\n" +
				"}");
		expect( typeError("boolVar", new IntegerType(), new BooleanType()),
				emptyMain+
				"class Classy {\n" +
				"   public int foo(boolean boolVar) {\n" +
				"      System.out.println(boolVar);\n" +
				"      return 0;\n" +
				"   }\n" +
				"}");
	}

	@Test public void assignLocalVar() throws Exception {
		String[] exps =  { "true",    "0",   "new Foo()", "new Bar()", "new int[4]" };
		Type[] types = { new BooleanType(), new IntegerType(), 
				new ObjectType("Foo"), new ObjectType("Bar"), new IntArrayType()};
		for (int i = 0; i < exps.length; i++) {
			for (int j = 0; j < types.length; j++) {
				Type type = types[i];
				String exp = exps[j];
				Type expType = types[j];
				
				String prog = 
					emptyMain+
					"class Foo {\n" +
					"   public int test() {\n" +
					"      "+type+" x;\n" +
					"      x = "+exp+";\n"+
					"      return 0;\n"+
					"   }\n" +
					"}\n" +
					"class Bar {\n" +
					"}";
				if (i==j) 
					accept(	prog );
				else
					expect( typeError(exp, type, expType), prog);
			}
		}
	}

	@Test public void assignField() throws Exception {
		String[] exps =  { "true",    "0",   "new Foo()", "new Bar()", "new int[4]" };
		Type[] types = { new BooleanType(), new IntegerType(), 
				new ObjectType("Foo"), new ObjectType("Bar"), new IntArrayType()};
		for (int i = 0; i < exps.length; i++) {
			for (int j = 0; j < types.length; j++) {
				Type type = types[i];
				String exp = exps[j];
				Type expType = types[j];
				
				String prog = 
					emptyMain+
					"class Foo {\n" +
					"   "+type+" x;\n" +
					"   public int test() {\n" +
					"      x = "+exp+";\n"+
					"      return 0;\n"+
					"   }\n" +
					"}\n" +
					"class Bar {\n" +
					"}";
				if (i==j) 
					accept(	prog );
				else
					expect( typeError(exp, type, expType), prog);
			}
		}
	}
	
	@Test public void arrayAssign() throws Exception {
		accept( emptyMain+
				"class Foo {\n" +
				"   int[] a;\n" +
				"   public int test() {\n" +
				"       a[2] = 0;\n" +
				"       return 0;\n"+
				"   }\n" +
				"}");
		
		expect( typeError("false", new IntegerType(), new BooleanType()),
				emptyMain+
				"class Foo {\n" +
				"   int[] a;\n" +
				"   public int test() {\n" +
				"       a[false] = 0;\n" +
				"       return 0;\n"+
				"   }\n" +
				"}");
		
		expect( typeError("a", new IntArrayType(), new IntegerType()),
				emptyMain+
				"class Foo {\n" +
				"   int a;\n" +
				"   public int test() {\n" +
				"       a[2] = 0;\n" +
				"       return 0;\n"+
				"   }\n" +
				"}");
		
		expect( typeError("false", new IntegerType(), new BooleanType()),
				emptyMain+
				"class Foo {\n" +
				"   int[] a;\n" +
				"   public int test() {\n" +
				"       a[2] = false;\n" +
				"       return 0;\n"+
				"   }\n" +
				"}");
		
		expect( ErrorMessage.undefinedId("a"),
				emptyMain+
				"class Foo {\n" +
				"   public int test() {\n" +
				"       a[2] = 0;\n" +
				"       return 0;\n"+
				"   }\n" +
				"}");
				
	}
	@Test public void fieldDeclAfterItsUse() throws Exception {
		accept(
			emptyMain+
			"class Foo {\n" +
			"   public int test() {\n" +
			"      x = 1;\n" +
			"      return x;\n"+
			"   }\n" +
			"   int x;\n"+
			"}");
	}
		
	@Test public void assignUnknownVar() throws Exception {
		expect( ErrorMessage.undefinedId("huh"),
				emptyMain +
				"class Foo {\n"+
				"   public int test() {\n" +
				"      huh = 4;\n"+
				"      return 0;\n"+
				"   }\n" +
				"}");
		expect( ErrorMessage.undefinedId("huh"),
				mainClass("huh = 4;"));
	}
	
	@Test public void testIf() throws Exception {
		accept(	mainClass("if (true) System.out.println(1); else System.out.println(0);"));
		
		expect(	typeError("99", new BooleanType(), new IntegerType()), 
				mainClass("if (99) System.out.println(1); else System.out.println(0);"));
		expect(	typeError("false", new IntegerType(), new BooleanType()), 
				mainClass("if (true) System.out.println(false); else System.out.println(0);"));
		expect(	typeError("false", new IntegerType(), new BooleanType()), 
				mainClass("if (true) System.out.println(1); else System.out.println(false);"));
	}

	@Test public void testWhile() throws Exception {
		accept(	mainClass(
				"while (true) { \n" +
				"   System.out.println(1);" +
				"}"));
		
		expect(	typeError("99", new BooleanType(), new IntegerType()), 
				mainClass(
						"while (99) { \n" +
						"   System.out.println(1);" +
						"}"));
		expect(	typeError("false", new IntegerType(), new BooleanType()), 
				mainClass(
						"while (true) { \n" +
						"   System.out.println(false);" +
						"}"));
	}
	
	//
	// Expressions
	//
	
	@Test public void badCall() throws Exception {
		String testClass = 
		"class Foo {\n" +
		"  public int foo(int x, boolean b) {" +
		"     return x;\n"+
		"  }\n"+
		"  public boolean bar(int x, boolean b) {" +
		"     return b;\n"+
		"  }\n";

		accept(	emptyMain+
				testClass+
				"  public int test() {\n" +
				"    System.out.println(this.foo(10,this.bar(20,true)));\n" +
				"    return 0;\n" +
				"  }\n"+
				"}");
		
		expect( ErrorMessage.undefinedId("undef"),
				emptyMain+
				testClass+
				"  public int test() {\n" +
				"    System.out.println(this.foo(10,this.undef(20,true)));\n" +
				"    return 0;\n" +
				"  }\n"+
				"}");
		
		expect( typeError("99", new BooleanType(), new IntegerType()),
				emptyMain+
				testClass+
				"  public int test() {\n" +
				"    System.out.println(this.foo(10,this.bar(20,99)));\n" +
				"    return 0;\n" +
				"  }\n"+
				"}");
		
		expect( typeError("this.bar(20,true)", new IntegerType(), new BooleanType()),
				emptyMain+
				testClass+
				"  public int test() {\n" +
				"    System.out.println(this.foo(this.bar(20,true),true));\n" +
				"    return 0;\n" +
				"  }\n"+
				"}");
		
		expect( wrongNumberOfArguments("this.foo()", 2),
				emptyMain+
				testClass+
				"  public int test() {\n" +
				"    System.out.println(this.foo());\n" +
				"    return 0;\n" +
				"  }\n"+
				"}");
		
		expect( typeErrorExpectObject("99", new IntegerType()),
				emptyMain+
				testClass+
				"  public int test() {\n" +
				"    System.out.println(99.foo());\n" +
				"    return 0;\n" +
				"  }\n"+
				"}");
	}
	

	@Test public void newObject() throws Exception {
		accept(	emptyMain+
				"class Foo {\n" +
				"   public int test() {\n"+
				"      Bar bar;\n" +
				"      bar = new Bar();\n"+
				"      return 0;\n"+
				"   }\n" +
				"}\n" +
				"class Bar {\n" +
				"   public int test() {\n"+
				"      Foo foo;\n" +
				"      foo = new Foo();\n"+
				"      return 0;\n"+
				"   }\n" +
				"}\n" 
		);
		expect(	ErrorMessage.undefinedId("FOo"),
				emptyMain+
				"class Foo {\n" +
				"   public int test() {\n"+
				"      Foo foo;\n" +
				"      foo = new FOo();\n"+
				"      return 0;\n"+
				"   }\n" +
				"}");
		expect(	typeError("new Bar()", new ObjectType("Foo"), new ObjectType("Bar")),
				emptyMain+
				"class Foo {\n" +
				"   public int test() {\n"+
				"      Foo foo;\n" +
				"      foo = new Bar();\n"+
				"      return 0;\n"+
				"   }\n" +
				"}\n" +
				"class Bar {\n" +
				"}");
	}
	
	@Test public void arrayLen() throws Exception {
		accept( progWithExp("int", "a.length"));
		
		expect( typeError("b", new IntArrayType(), new BooleanType()),
				progWithExp("int", "b.length"));
		
		expect( typeError("a.length", new BooleanType(), new IntegerType()),
				progWithExp("boolean", "a.length"));
	}
	
	@Test public void arithOps() throws Exception {
		String[] ops = { "+", "-", "*" };
		for (int i = 0; i < ops.length; i++) {
			String op = ops[i];
			accept( progWithExp("int", "i" +op+"10"));
			accept( progWithExp("int", "10"+op+"i"));
			
			expect( typeError("b", new IntegerType(), new BooleanType()),
					progWithExp("int", "b"+op+"10") );
			expect( typeError("b", new IntegerType(), new BooleanType()),
					progWithExp("int","10"+op+"b") );
			expect( typeError("10"+op+"i", new BooleanType(), new IntegerType()),
					progWithExp("boolean", "10"+op+"i"));
			
		}
	}
	
	@Test public void lessThan() throws Exception {
		String op = "<";
		accept( progWithExp("boolean", "i" +op+"10"));
		accept( progWithExp("boolean", "10"+op+"i"));
		accept( progWithExp("boolean", "i+10" +	op + "2*i"));

		expect( typeError("b", new IntegerType(), new BooleanType()),
				progWithExp("boolean", "b"+op+"10") );
		expect( typeError("b", new IntegerType(), new BooleanType()),
				progWithExp("boolean","10"+op+"b") );
		expect( typeError("10"+op+"i", new IntegerType(), new BooleanType()),
				progWithExp("int", "10"+op+"i"));
	}
	
	@Test public void andand() throws Exception {
		String op = "&&";
		accept( progWithExp("boolean", "b" +op+"false"));
		accept( progWithExp("boolean", "false"+op+"b"));
		accept( progWithExp("boolean", "i<10" +	op + "5<i"));

		expect( typeError("i", new BooleanType(), new IntegerType()),
				progWithExp("boolean", "i"+op+"true") );
		expect( typeError("i",new BooleanType(), new IntegerType()),
				progWithExp("boolean","false"+op+"i") );
		
		expect( typeError("true"+op+"b", new IntegerType(), new BooleanType()),
				progWithExp("int", "true"+op+"b"));
	}
	
	@Test public void not() throws Exception {
		accept( progWithExp("boolean", "!b"));
		accept( progWithExp("boolean", "!true"));
		accept( progWithExp("boolean", "!false"));

		expect( typeError("i", new BooleanType(), new IntegerType()),
				progWithExp("boolean", "!i") );
		expect( typeError("!b", new IntegerType(), new BooleanType()),
				progWithExp("int", "!b") );
	}
	
	@Test public void arrayLookup() throws Exception {
		accept( progWithExp("int", "a[i]"));
		
		expect( typeError("b", new IntArrayType(), new BooleanType()),
				progWithExp("int", "b[i]"));
		
		expect( typeError("b", new IntegerType(), new BooleanType()),
				progWithExp("int", "a[b]"));
		
		expect( typeError("a[i]", new BooleanType(), new IntegerType()),
				progWithExp("boolean", "a[i]"));
	}
	
	///// Tests for inheritance implementation ////////////////////////////////////
	/// Implementing support for inheritance is optional.
	/// If you chose not to implement inheritance these test
	/// are not supposed to pass.
	
	@Test public void inheritanceNoOverriding() throws Exception {
		accept( emptyMain + 
				"class C extends B { " +
				"   int c;\n" +
				"   public int getC() { return c; }\n" +
				"   public int test() {\n" +
				"      return this.getC()+this.getB()+this.getA();\n"+
				"   }\n" +
				"}\n" +
				"class B extends A { " +
				"   int b;\n" +
				"   public int getB() { return b; }\n" +
				"}\n" +
				"class A { " +
				"   int a;\n" +
				"   public int getA() { return a; }\n" +
				"}");
	}
	
	@Test public void inheritanceWithMethodOverriding() throws Exception {
		accept( emptyMain + 
				"class C extends B { " +
				"   int c;\n" +
				"   public int getC() { return c; }\n" +
				"   public int test(int a, boolean b) { \n"+
				"      return this.getC()+this.getB()+this.getA();\n"+
				"   }\n" +
				"}\n" +
				"class B extends A { " +
				"   int b;\n" +
				"   public int getA() { return a+1; }\n" +
				"   public int getB() { return b; }\n" +
				"   public int test(int a, boolean b) { \n"+
				"      return this.getB()+this.getA();\n"+
				"   }\n" +
				"}\n" +
				"class A { " +
				"   int a;\n" +
				"   public int getA() { return a; }\n" +
				"   public int test(int i, boolean j) { \n"+
				"      return this.getA();\n"+
				"   }\n" +
				"}");
		//Overriding is only allowed if method signatures match exactly
		expect( ErrorMessage.badMethodOverriding(new ObjectType("Foo"), "test"),
				emptyMain+ 
				"class Foo extends Bar {\n" +
				"   public int test(boolean b, int i) { return i; }\n" +
				"}\n" +
				"class Bar {\n" +
				"   public int test(int i, boolean b) { return i; }\n" +
				"}");
		expect( ErrorMessage.badMethodOverriding(new ObjectType("Foo"), "test"),
				emptyMain+ 
				"class Foo extends Bar {\n" +
				"   public boolean test(int i, boolean b) { return i; }\n" +
				"}\n" +
				"class Bar {\n" +
				"   public int test(int i, boolean b) { return i; }\n" +
				"}");
		expect( ErrorMessage.badMethodOverriding(new ObjectType("Foo"), "test"),
				emptyMain+ 
				"class Foo extends Bar {\n" +
				"   public int test(int i) { return i; }\n" +
				"}\n" +
				"class Bar {\n" +
				"   public int test(int i, int b) { return i; }\n" +
				"}");
		expect( ErrorMessage.badMethodOverriding(new ObjectType("Foo"), "test"),
				emptyMain+ 
				"class Foo extends Bar {\n" +
				"   public int test(int i, int b) { return i; }\n" +
				"}\n" +
				"class Bar {\n" +
				"   public int test(int i) { return i; }\n" +
				"}");
	}
	@Test public void inheritanceWithFieldOverriding() throws Exception {
		// Field overriding is not allowed 
		expect( ErrorMessage.fieldOverriding(new ObjectType("Foo"), "test"), 
				emptyMain+ 
				"class Foo extends Bar {\n" +
				"   int test;\n" +
				"}\n" +
				"class Bar {\n" +
				"   int test;\n" +
				"}");
		expect( ErrorMessage.fieldOverriding(new ObjectType("Foo"), "test"), 
				emptyMain+ 
				"class Foo extends Middle {\n" +
				"   int test;\n" +
				"}\n" +
				"class Middle extends Bar {}\n"+
				"class Bar {\n" +
				"   int test;\n" +
				"}");
	}
	
	///////////////////////// Helpers /////////////////////////////////////////////
	
	private ErrorMessage typeError(String exp, Type expected, Type actual) throws ParseException {
 		return ErrorMessage.typeError(parseExp(exp), expected, actual);
	}
	
	private ErrorMessage wrongNumberOfArguments(String callExp, int expected) throws ParseException {
		return ErrorMessage.wrongNumberOfArguments((Call)parseExp(callExp), expected);
	}

	private ErrorMessage typeErrorExpectObject(String exp, Type actualType) throws ParseException {
		return ErrorMessage.typeErrorExpectObject(parseExp(exp), actualType);
	}
	
	private void accept(File file) throws TypeCheckerException, Exception {
		MiniJavaTypeChecker.parseAndCheck(file); 
	}
	
	private void accept(String string) throws TypeCheckerException, Exception {
		MiniJavaTypeChecker.parseAndCheck(string); 
	}

	/**
	 * Mostly what we want to do in this set of unit tests is see whether the checker
	 * produces the right kind of error reports. This is a helper method to do just that.
	 */
	private void expect(ErrorMessage expect, String input) throws Exception {
		try {
			MiniJavaTypeChecker.parseAndCheck(input);
			Assert.fail("A TypeCheckerException should have been raised but was not.");
		}
		catch (TypeCheckerException e) {
			Assert.assertEquals(expect, e.getFirstMessage());
		}
	}

	////////// Various code snippets and templates to be used in our tests //////////////
	
	private static final String emptyMain = mainClass("{}");
	
	private static String mainClass(String stm) {
		return
		"class Main { \n" +
		"   public static void main(String[] args) {\n" +
		"      "+stm+"\n"+
		"   }\n" +
		"}\n";
	}
	
	/**
	 * Generate a test program with an expression in it.
	 */
	private String progWithExp(String type, String exp) {
		return emptyMain +
			   "class Foo {\n" +
			   "   int i;\n"+
			   "   int[] a;\n" +
			   "   boolean b;\n" +
			   "   public int foo(int p1, int p2) {\n" +
			   "      "+type+" result;\n"+
			   "      result = "+exp + ";\n" +
			   "      return 0;\n" +
			   "   }\n"+
			   "}";
	}

}
