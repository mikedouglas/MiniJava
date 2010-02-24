package minijava.ir.interp;

import java.util.HashMap;

import junit.framework.Assert;
import minijava.ir.canon.BasicBlocks;
import minijava.ir.temp.Label;
import minijava.ir.tree.IRStm;
import minijava.ir.tree.LABEL;
import minijava.translate.ProcFragment;
import minijava.util.List;

public class SimProc extends Callable {

	private ProcFragment frag;
	private HashMap<Label, List<IRStm>> labels = new HashMap<Label, List<IRStm>>();
	private List<IRStm> start;
	
	//When using the basic blocks setup, the code should finish by executing a JUMP to
	//this special label:
	private Label doneLabel;
	
	public SimProc(ProcFragment methodFrag, InterpMode setup) {
		this.frag = methodFrag;
		switch (setup) {
		case LINEARIZED_IR:
			init(frag.getLinearizedBody());
			break;
		case BASIC_BLOCKS:
			init(frag.getBasicBlocks());
			break;
		case TRACE_SCHEDULE:
			init(frag.getTraceScheduledBody());
			break;
		default:
			throw new Error("Missing case?");
		}
	}

	private void init(BasicBlocks bb) {
		List<List<IRStm>> basicBlocks = bb.blocks;
		doneLabel = bb.doneLabel;
		this.start = basicBlocks.head(); 
		for (List<IRStm> basicBlock : basicBlocks) {
			//First statement in the basic block should be a label
			Label l = ((LABEL) basicBlock.head()).getLabel();
			putLabel(l, basicBlock.tail());
		}
	}

	private void init(List<IRStm> program) {
		this.start = program;
		for (List<IRStm> stms = start; !stms.isEmpty(); stms = stms.tail()) {
			IRStm currentStm = stms.head();
			if (currentStm instanceof LABEL) {
				putLabel(((LABEL)currentStm).getLabel(), stms.tail());
			}
		}
	}

	private void putLabel(Label label, List<IRStm> tail) {
		List<IRStm> existing = labels.get(label); 
		Assert.assertNull("Duplicate label in IR code", existing);
		labels.put(label, tail);
	}

	@Override
	public Word call(Interp interp, List<Word> args) {
		List<IRStm> instructionPtr = start;
		X86SimFrame frame = frag.getFrame().newSimFrame(interp, args);
		while (!instructionPtr.isEmpty()) {
			 Label jumpTo = instructionPtr.head().interp(frame);
			 if (jumpTo==null)
				 instructionPtr = instructionPtr.tail();
			 else if (jumpTo==doneLabel)
				 return frame.getReturnValue();
			 else 
				 instructionPtr = labels.get(jumpTo);
		}
		if (doneLabel==null) {
			// Not using basic blocks, normal termination is by "getting to the end"
			return frame.getReturnValue();
		}
		else {
			// since the done label was defined, we expect that "correct" IR code 
			// should use it to jump out of the method body.
			throw new Error("Simulation of IR procedure/method body ended unexpectedly");
		}
	}
	
	@Override
	public String toString() {
		return "SimProc("+frag.getLabel()+")";
	}

}
