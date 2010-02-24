package minijava.test.translate;

import java.io.File;

import junit.framework.Assert;
import minijava.ir.frame.Frame;
import minijava.ir.frame.x86.X86Frame;
import minijava.ir.interp.Interp;
import minijava.ir.interp.InterpMode;
import minijava.test.SampleCode;
import minijava.translate.Fragments;
import minijava.translate.Translator;
import minijava.typechecker.TypeCheckerException;
import minijava.util.Utils;

import org.junit.Ignore;
import org.junit.Test;

/**
 * Test the minijava translation phase that takes a (type-checked) program and turns
 * the bodies of all the methods in the program into IRtrees.
 * <p>
 * This test suite uses the IR interpreter to simulate the execution of the
 * resulting IR. This gives us some confidence that our tranlation works correctly :-)
 * 
 * @author kdvolder
 */
public class TestTranslate {

	public static final Frame architecture = X86Frame.factory;

	/**
	 * To make it easy to run all of these tests with the either 
	 * linearized ir code, basic blocks or trace scheduled code
	 * We determine the simulation mode via this method.
	 * <p>
	 * Simply creating a subclass and overriding this method will create
	 * a test suite that runs all the same tests in a different simulation 
	 * mode.
	 * 
	 * @return
	 */
	protected InterpMode getSimulationMode() {
		return InterpMode.LINEARIZED_IR;
	}

	/**
	 * Print out all the generated IR?
	 * <p>
	 * If false, only the result of simulating the IR execution 
	 * will be printed.
	 */
	protected boolean dumpIR() {
		return true;
	}

	private static int testNumber = 0;
	
	@Test
	public void simpleProgram() throws Exception {
		test(	"",
				"class Main {\n" +
				"  public static void main(String[] args) { {} }\n"+
				"}"
		);
	}
	
	@Test 
	public void printNumber() throws Exception {
		test("10\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(10);\n"+
				"  }\n" +
				"}"
		);
	}
	
	@Test public void printSum() throws Exception {
		test("30\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(10+20);\n"+
				"  }\n" +
				"}"
		);
	}
	
	@Test public void simpleMethod() throws Exception {
		test("",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      {}\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int getFoo() { return 0; }\n"+
				"}"
		);
	}
		
	@Test public void callSimpleMethod() throws Exception {
		test("78\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Foo().getFoo());\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int getFoo() { return 78; }\n"+
				"}"
		);
	}
	
	@Test public void callSimpleMethodWithArgs() throws Exception {
		test("1697\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Foo().sum(14,17));\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int sum(int x, int y) { return x+y*99; }\n"+
				"}"
		);
	}
	
	@Test public void simpleMethodArgsAndLocals() throws Exception {
		test("1711\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Foo().sum(14,17));\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int sum(int x, int y) { \n" +
				"       int tmp;\n"+
				"       int tmp2;\n"+
				"       tmp = y*99;\n"+
				"       tmp2 = x*2;\n"+
				"       return tmp+tmp2;\n" +
				"   }\n"+
				"}"
		);
	}
	
	@Test public void simpleArrays() throws Exception {
		test( "99\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Foo().do());\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int do() { \n" +
				"      int[] arr;\n" +
				"      arr = new int[1];\n"+
				"      arr[0] = 99;\n"+
				"      return arr[0];\n"+
				"   }\n"+
				"}"
		);
	}
	@Test public void arrays() throws Exception {
		test( (17+2*17+2)+"\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Foo().getSet(new int[2],17));\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int getSet(int[] arr, int y) { \n" +
				"      arr[0] = y;\n"+
				"      arr[1] = 2*arr[0];\n"+
				"      return arr[0]+arr[1]+arr.length;\n"+
				"   }\n"+
				"}"
		);
	}
	
	@Test public void printArg() throws Exception {
		test("10\n10\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Foo().parg(10));\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int parg(int arg) { \n" +
				"      System.out.println(arg);\n"+
				"      return arg;\n"+
				"   }\n"+
				"}"
		);
	}
	@Test public void printWhile() throws Exception {
		test("5\n4\n3\n2\n1\n0\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Foo().warg(5));\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int warg(int arg) { \n" +
//				"      System.out.println(901);\n" +
				"      while (0<arg) {\n"+
//				"         System.out.println(902);\n" +
				"         System.out.println(arg);\n" +
				"         arg = arg - 1;\n"+
				"      }\n"+
//				"      System.out.println(903);\n" +
				"      return arg;\n"+
				"   }\n"+
				"}"
		);
	}
	@Test public void whileFac() throws Exception {
		test("24\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Foo().fac(4));\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int fac(int n) { \n" +
				"      int fac;\n"+
				"      int i;\n"+
				"      i = n; fac = 1; \n"+
				"      while (0<i) { fac = fac*i; i = i-1; } \n"+ 
				"      return fac;\n"+
				"   }\n"+
				"}"
		);
	}
	
	@Test public void arrayLoop() throws Exception {
		test( (11*10/2)+"\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Foo().doit(new int[10]));\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int doit(int[] arr) { \n" +
				"      int dum;\n" +
				"      dum = this.init(arr);\n"+
				"      return this.sum(arr);\n"+
				"   }\n"+
				"   public int init(int[] arr) {\n"+
				"      int i;\n" +
				"      i = 0;\n" +
				"      while (i < arr.length) {\n" +
				"         arr[i] = i+1;\n"+
				"         i = i + 1;\n"+
				"      }\n" +
				"      return 0;\n"+
				"   }\n"+
				"   public int sum(int[] arr) {\n"+
				"      int i;\n" +
				"      int sum;\n" +
				"      i = 0; sum = 0;\n" +
				"      while (i < arr.length) {\n" +
				"         sum = sum + arr[i];\n"+
				"         i = i + 1;\n"+
				"      }\n" +
				"      return sum;\n" +
				"   }\n"+
				"}"
		);
	}

	@Test public void ifThenElse() throws Exception {
		test( "20\n20\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      { System.out.println(new Foo().max(10,20));\n" +
				"        System.out.println(new Foo().max(20,10));\n" +
				"      }\n"+
				"  }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int max(int x, int y) { \n" +
				"      int tmp;\n"+
				"      if (x<y) tmp = y; else tmp = x;\n"+
				"      return tmp;\n"+
				"   }\n"+
				"}"
		);
	}
	@Test public void flag() throws Exception {
		test( "99\n22\n",
				"class Main {\n" +
				"  public static void main(String[] args) { {\n" +
				"      System.out.println(new Foo().doit(10,20));\n" +
				"      System.out.println(new Foo().doit(20,10));\n" +
				"  } }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int doit(int x, int y) {\n" +
				"      int result;\n"+
				"      if (this.ordered(x, y))\n" +
				"         result = 99;\n" +
				"      else\n" +
				"         result = 22;\n"+
				"      return result;\n"+
				"   }\n" +
				"   public boolean ordered(int x, int y) { \n" +
				"      return x<y;\n"+
				"   }\n"+
				"}"
		);
	}
	
	@Test public void whileLoop() throws Exception {
		test( (10*9/2)+"\n",
				"class Main {\n" +
				"  public static void main(String[] args) { {\n" +
				"      System.out.println(new Foo().doit(10));\n" +
				"  } }\n" +
				"}\n" +
				"class Foo {\n " +
				"   public int doit(int x) {\n" +
				"      int result;\n"+
				"      int count;\n"+
				"      result = 0;\n" +
				"      count = 1;\n"+
				"      while (count<x) {\n" +
				"          result = result + count;\n" +
				"          count = count + 1;\n" +
				"      }\n"+
				"      return result;\n"+
				"   }\n" +
				"}"
		);
		
	}
	

	@Test public void field() throws Exception {
		test( "50\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Point().doit());\n" +
				"  }\n" +
				"}\n" +
				"class Point {\n " +
				"   int x; int y;\n"+
				"   public int doit() {\n" +
				"      x = 10;\n"+
				"      y = 20;\n"+
				"      return x+2*y;\n"+
				"   }\n" +
				"}"
		);

	}
	
	@Test public void separateObjs() throws Exception {
		test( "1\n2\n10\n20\n0\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Test().doit());\n" +
				"  }\n" +
				"}\n" +
				"class Test {\n" +
				"   public int doit() {\n" +
				"      Point p1; Point p2;\n" +
				"      int ignore;\n" +
				"      p1 = new Point().init(1,2);\n" +
				"      p2 = new Point().init(10,20);\n" +
				"      ignore = p1.print();\n" +
				"      return p2.print();\n" +
				"   }\n"+
				"}\n" +
				"class Point {\n" +
				"   int x; int y;\n"+
				"   public Point init(int ix, int iy) {\n" +
				"      x = ix;\n"+
				"      y = iy;\n"+
				"      return this;\n"+
				"   }\n" +
				"   public int print() {\n" +
				"      System.out.println(x);\n"+
				"      System.out.println(y);\n"+
				"      return 0;\n"+
				"   }\n"+
				"}"
		);
	}
	
	@Test public void complexBranches() throws Exception {
		//Trying to create a program that has many choices... so many traces.
		//We want to work the trace schedule to a point where it actually 
		//gets driven into some of the rarer cases.
		test( "1\n7\n",
		"class Main {\n" +
		"  public static void main(String[] args) {\n" +
		"      System.out.println(new Test().doit());\n" +
		"  }\n" +
		"}\n" +
		"class Test {\n" +
		"   int count;\n"+
		"   public boolean cond() {\n" +
		"      count = count + 1;\n"+
		"      return true;\n"+
		"   }\n"+
		"   public int doit() {\n" +
		"     boolean a;\n"+
		"     a = this.cond() && this.cond() && this.cond();\n"+
		"     if (a && this.cond() && this.cond() && this.cond() && this.cond())\n" +
		"        System.out.println(1);" +
		"     else\n"+
		"        System.out.println(0);" +
		"     return count;\n" +
		"   }\n"+
		"}");
	}

	@Test public void emptyBranches() throws Exception {
		//This test is good to see if the BasicBlocks / TraceScheduler deal well
		//with "empty" basic blocks (they do *not* => inefficient jumps)
		//Challenge problem: inspect the code after TraceScheduling and try to fix
		//the compiler somehow to produce more optimal code.
		test( "9999\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Test().test(true));\n" +
				"  }\n" +
				"}\n" +
				"class Test {\n"+
				"  public int test(boolean flag) {\n" +
				"     if (flag) {} else {}\n"+
				"     return 9999;\n"+
				"  }\n" +
				"}");
	}
	@Test public void emptyElseBranche() throws Exception {
		//This test is good to see if the BasicBlocks / TraceScheduler deal well
		//with "empty" basic blocks (they do not => inefficient jumps)
		//Challenge problem: inspect the code after TraceScheduling and try to fix
		//the compiler somehow to produce more optimal code.
		test( "99\n9999\n",
				"class Main {\n" +
				"  public static void main(String[] args) {\n" +
				"      System.out.println(new Test().test(true));\n" +
				"  }\n" +
				"}\n" +
				"class Test {\n" +
				"  public int test(boolean flag) {\n" +
				"      if (true) System.out.println(99); else {}\n" +
				"      return 9999;\n"+
				"  }\n" +
				"}");
	}
	
	//////////////// Sample code //////////////////////////////////
	
	@Test
	public void testSampleCode() throws Exception {
		File[] files = SampleCode.sampleFiles();
		for (int i = 0; i < files.length; i++) {
			File f = files[i];
			if (!optionalSample(f))
				test(f);
		}
	}
	@Test @Ignore // Don't run this unless you are implementing inheritance support!
	public void testOptionalSampleCode() throws Exception {
		File[] files = SampleCode.sampleFiles();
		for (int i = 0; i < files.length; i++) {
			File f = files[i];
			if (optionalSample(f))
				test(f);
		}
	}
	
	protected Fragments test(File program) throws TypeCheckerException, Exception {
		System.out.println("Translating: "+program);
		String outFileName = program.getPath();
		outFileName = outFileName.substring(0, outFileName.length()-".java".length())
					+".out";
		String expected = Utils.getContents(new File(outFileName));
		
		return test(expected, program);
	}	

	protected Fragments test(String expected, File program)
			throws TypeCheckerException, Exception {
		Fragments translated = Translator.translate(architecture, program);
		if (dumpIR()) {
			System.out.println("VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV");
			System.out.println(translated);
			System.out.println();
		}
			
		if (getSimulationMode()!=null) {
			System.out.println("Simulating IR code:");
			Interp interp = new Interp(translated, getSimulationMode());
			String result = interp.run();
			System.out.println(result);
			Assert.assertEquals(expected, result);
		}
		System.out.println("=================================");
		return translated;
	}
	
	private boolean optionalSample(File f) {
		return f.toString().endsWith("Visitor.java");
	}
	
	protected Fragments test(String expected, String program) throws Exception {
		System.out.println("Translating program: ");
		System.out.println(program);
		Fragments translated = Translator.translate(architecture, program);
		if (dumpIR()) {
			System.out.println("VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV");
			System.out.println(translated);
			System.out.println();
		}
		if (getSimulationMode()!=null) {
			System.out.println("Simulating IR code:");
			Interp interp = new Interp(translated, getSimulationMode());
			String result = interp.run();
			System.out.print(result);
			Assert.assertEquals(expected, result);
		}
		System.out.println("=================================");
		return translated;
	}

}
