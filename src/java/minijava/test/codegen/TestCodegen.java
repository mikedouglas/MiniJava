package minijava.test.codegen;

import java.io.File;

import minijava.codegen.CodeGenerator;
import minijava.codegen.x86.X86Muncher;
import minijava.ir.interp.InterpMode;
import minijava.test.translate.TestTranslate;
import minijava.translate.Fragments;
import minijava.typechecker.TypeCheckerException;

import org.junit.AfterClass;

/**
 * Unfortunately, there's no good way that I can think of to test this phase
 * reliably. 
 * <p>
 * So what we will do is simply to compile again all the TestTranslate programs into
 * IR code, then convert these into assem instructions, and then print out the
 * result.
 * <p>
 * Since assem instructions are really just a kind of Strings we can't simulate their
 * execution. We also can't really compare the output to a "golden transcript" because
 * their is too much variability in what the end result may look like (the IR may already
 * look different to start with, and converting IR into assembly instructions can
 * also be done in many ways.
 * <p>
 * The test provided here is therefore not much more than a test that your code produces
 * some output without crashing, not whether this output is correct in any sense.
 * 
 * @author kdvolder
 */
public class TestCodegen extends TestTranslate {
	
	// Note that this class doesn't actually contain any @Test methods.
	// It inherits them from TestTranslate.
	// Each test will call one of the two test methods below, which we override 
	// here.
	// This way we can easily reuse all the tests, but make them do something extra
	// (i.e. generate and print the assembly).
	
	@Override
	protected InterpMode getSimulationMode() {
		return null; // means don't simulate IR code.
	}
	
	@Override
	protected Fragments test(File program) throws TypeCheckerException, Exception {
		Fragments translated = super.test(program);
		System.out.println("Generating assembly for "+program);
		test(translated);
		return null; // doesn't matter right now.
	}
	
	@Override
	protected Fragments test(String expected, String program) throws Exception {
		Fragments translated = super.test(expected, program);
		System.out.println("Generating assembly for "+program);
		test(translated);
		return null; // doesn't matter right now.
	}
	
	protected void test(Fragments ir_fragments) {
		CodeGenerator cogen = new CodeGenerator();
		System.out.println(cogen.apply(ir_fragments));
	}

	/**
	 * This prints out a representation of the X86Muncher's pattern munching rules.
	 * For debugging purposes. 
	 */
	@AfterClass public static void dumpRules() {
	//	X86Muncher.dumpRules();
	}

}
