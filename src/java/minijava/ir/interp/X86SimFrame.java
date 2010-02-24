package minijava.ir.interp;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import junit.framework.Assert;
//import minijava.ir.frame.x86.X86Frame;
import minijava.ir.temp.Label;
import minijava.ir.temp.Temp;
import minijava.ir.tree.IRExp;
import minijava.util.DefaultIndentable;
import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * Represents a simulated stack frame on the X86 architecture.
 * <p>
 * This serves as a kind of "environment" for the interpretation
 * if IR code in the IR interpreter. 
 * <p>
 * This class contains much code that is probably reusable for 
 * different architectures. It may be worthwhile to refactor it
 * so the interpreter code use an abstract class SimFrame and the
 * X86Frame (or Frame implementations for other architectures) returns
 * a concrete instance of X86SimFrame which inherits much of the
 * reusable code from the abstract superclass.
 * <p>
 * We haven't done this yet because the cs411 project only calls
 * for an X86 implementation.
 */
public class X86SimFrame extends DefaultIndentable {
	
	private Map<Temp, Word> temps = new HashMap<Temp, Word>();

	// Compile time counterpart of this frame (has some crucial information such
	// as wordSize and the names for special Temps to store the RV and the FP).
	private final X86Frame ct_frame;
	
	// This represents the area of the stack frame where formals are
	// stored (in x86 all formals are on the stack).
	private Array frameBytes;

	// A pointer to the interpreter for this program (needed to be able
	// to simulate calls to other procedures).
	private Interp interp;
	
	public X86SimFrame(Interp interp, X86Frame frame, List<Word> args) {
		this.interp = interp;
		this.ct_frame = frame;
		this.frameBytes = new Array(args.size()+2+frame.numLocals(), frame.wordSize());

		Ptr currentFormal = framePtr().add(X86Frame.FIRST_FORMAL_OFFSET);
		for (int i = 0; i < args.size(); i++) {
			currentFormal.set(args.get(i));
			currentFormal = currentFormal.add(X86Frame.FORMAL_INCREMENT);
		}
		framePtr().set(FP().interp(this));
		FP().set(framePtr(), this);
		framePtr().add(frame.wordSize()).set(new UninitializedWord("?return"));
	}

	// Retrieve the value of a given temp variable. If the temp
	// variable was not written to in the course of executing the
	// method, the UninitializedWord will be returned.
	// 
	// @param name
	// @return
	public Word getTemp(Temp name) {
		Word result = temps.get(name);
		if (result==null) {
			return new UninitializedWord("?"+name);
		}
		return result;
	}
	
	// Write a value into a given Temp. The value must be a real
	// value (i.e. not a Java null pointer or the UninitializedWord
	public void setTemp(Temp name, Word value) {
		Assert.assertFalse(value==null);
		temps.put(name, value);
	}

	// Return snippet of IR code that computes the frame pointer.
	// (e.g. in x86 this snippet of code references the %ebp
	// register).
	public IRExp FP() {
		return ct_frame.FP();
	}

	// Return a pointer to the frame's base (the value that should be
	// stored in FP during IR simulation on procedure entry.
	public Ptr framePtr() {
		//adjust for the space taken by the local variables.
		return frameBytes.add(ct_frame.numLocals()*ct_frame.wordSize());
	}

	public Word getReturnValue() {
		return RV().interp(this);
	}

	private IRExp RV() {
		return ct_frame.RV();
	}

	public Interp getInterp() {
		return interp;
	}

	@Override
	public void dump(IndentingWriter out) {
		out.println("X86SimFrame {");
		out.indent();
		
		out.print("frame : ");
		out.println(framePtr());
		
		out.println("temps : ");
		out.indent();
		for (Entry<Temp, Word> entry : temps.entrySet()) {
			out.print(entry.getKey() + " = ");
			out.println(entry.getValue());
		}
		out.outdent();
		
		out.outdent();
		out.print("}");
	}

	public Word getLabel(Label label) {
		Callable proc = interp.getProcLabel(label);
		if (proc!=null) 
			return proc;
		else {
			return new LabelPtr(label);
		}
	}

}
