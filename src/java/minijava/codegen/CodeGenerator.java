package minijava.codegen;

import minijava.translate.Fragment;
import minijava.translate.Fragments;
import minijava.translate.ProcFragment;


/**
 * This is the "interface" for the CodeGenerator phase of our compiler.
 * <p>
 * You can change (yada yada yada ... you know this by now :-)
 * <p>
 * However, the implementation provided here is actually "complete". It is built 
 * on top of a generic "MuncherRules" infrastructure. To implement a target architecture 
 * specific CodeGenerators. You only need to provide a factory method "newMuncher" in 
 * your frame class that creates a new Muncher initialized with MunchRules that are
 * specific to the target architecture you want to generate code for.
 */
public class CodeGenerator {

	/**
	 * Convert a program from IR into assembly code using this code generator.
	 */
	public Assembly apply(Fragments ir_fragments) {
		Assembly assembly = new Assembly();
		for (Fragment fragment : ir_fragments) {
			if (fragment instanceof ProcFragment) {
				assembly.add(apply((ProcFragment)fragment));
			}
			else
				throw new Error("Code generator doesn't know about fragments of this type");
		}
		return assembly;
	}

	private AssemFragment apply(ProcFragment fragment) {
		return new AssemProc(fragment);
	}

}
