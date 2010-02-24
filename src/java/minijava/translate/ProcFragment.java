package minijava.translate;

import minijava.ir.canon.BasicBlocks;
import minijava.ir.canon.Canon;
import minijava.ir.canon.TraceSchedule;
import minijava.ir.frame.Frame;
import minijava.ir.temp.Label;
import minijava.ir.tree.IRStm;
import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * A Method fragment represent the result of transforming a
 * minijava method declaration into IR code. 
 */
public class ProcFragment extends Fragment {

	// Note that we keep all the different versions of the IR and Assem code.
	// In production code, you will only keep the "last" result
	// produced after trace scheduling. Even that could be thrown
	// away after the assembly has been produced to a file.
	// But having the different version here, though it takes up
	// memory is rather useful when you are debugging the compiler.
	
	private Frame frame;
	
	/**
	 * The body of the method as returned by the translator.
	 * Not processed in any way.
	 */
	private IRStm body;
	
	/**
	 * The result of linearising body.
	 */
	private List<IRStm> linearizedBody;
	
	/**
	 * The result of applying the basic blocks algorithm to 
	 * linearizedBody.
	 */
	private BasicBlocks blocks;

	/**
	 * The result of applying the trace scheduling algorithm to 
	 * the basic blocks.
	 */
	private List<IRStm> traceScheduled;
	
	public ProcFragment(Frame frame, IRStm body) {
		this.frame = frame;
		this.body = body;
	}
	
	public List<IRStm> getLinearizedBody() {
		if (linearizedBody==null) {
			linearizedBody = Canon.linearize(body);
		}
		return linearizedBody;
	}
	
	public BasicBlocks getBasicBlocks() {
		if (blocks==null) {
			blocks = new BasicBlocks(getLinearizedBody());
		}
		return blocks;
	}

	public List<IRStm> getTraceScheduledBody() {
		if (traceScheduled==null) {
			traceScheduled = new TraceSchedule(getBasicBlocks()).getProgram();
		}
		return new TraceSchedule(getBasicBlocks()).getProgram();
	}
	

	@Override
	public void dump(IndentingWriter out) {
		out.println("MethodFragment {");
		out.indent();
		
		out.println(frame);
		
		out.println("body:");
		out.println(getLinearizedBody());
//		out.println(body);
		
//		out.println("trace scheduled IR:");
//		out.println(getTraceScheduledBody());
//		else if (blocks!=null) {
//			out.println("basic blocks:");
//			out.println(blocks);
//		}
//		else if (linearizedBody!=null) {
//			out.println("linearized IR:");
//			out.println(linearizedBody);
//		}
//		else {
//			out.println("Raw IR:");
//			out.println(body);
//		}
		
		out.outdent();
		out.print("}");
	}

	public Label getLabel() {
		return frame.getLabel();
	}

	public int wordSize() {
		return frame.wordSize();
	}

	public Frame getFrame() {
		return frame;
	}

}
