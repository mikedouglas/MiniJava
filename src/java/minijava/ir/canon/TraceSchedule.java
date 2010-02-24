package minijava.ir.canon;

import static minijava.ir.tree.IR.*;

import java.util.HashMap;
import java.util.Map;

import junit.framework.Assert;
import minijava.ir.temp.Label;
import minijava.ir.tree.CJUMP;
import minijava.ir.tree.IRStm;
import minijava.ir.tree.JUMP;
import minijava.ir.tree.LABEL;
import minijava.util.List;

public class TraceSchedule {

	/**
	 * We will be rebuilding the IR code in here, from the basic blocks.
	 */
	private List<IRStm> program = List.empty();
	
	private Map<Label, List<IRStm>> table = new HashMap<Label, List<IRStm>>();
	private Label doneLabel;

	public TraceSchedule(BasicBlocks bb) {
		List<List<IRStm>> theBlocks = bb.blocks;
		this.doneLabel = bb.doneLabel;
		//First we build a map to easily find a block corresponding to
		//a given label. This map also serves to remember which blocks
		//have already been "covered" (when covered we remove the entry
		//from the map).
		for (List<IRStm> block : theBlocks)
			table.put(getLabel(block), block);
		
		// The next loop ensures that any block gets covered by at least
		// one trace:
		for (List<IRStm> block : theBlocks) {
			trace(getLabel(block));
		}
		
		getProgram().add(LABEL(doneLabel));
		
		// The following code can be removed if you are confident this
		// algorithm is correctly implemented. It is just a
		// scan of the IR code to check that it looks right. 
		verify(getProgram(), bb.doneLabel);
		
	}
	
	/**
	 * Follow a trace starting at a given block, adding all the instructions 
	 * covered by the trace into the program. 
	 * <p>
	 * The trace ends if we reach a block that's already covered,
	 * or if we reach a jump to a label not in the IR code itself.
	 * The latter should happen only when we reach a 
	 * JUMP(doneLabel) instruction added by the basic blocks algorithm.
	 */
	void trace(Label startAt) {
		List<IRStm> block = table.get(startAt);
		if (block!=null) {
			table.remove(startAt); // only trace each block once!
			for (; !block.tail().isEmpty(); block = block.tail()) {
				//Loop through all statements except the last one.
				IRStm stm = block.head();
				Assert.assertFalse("Basic blocks algorithm broken? Jumps inside a BB!", stm.isJump());
				getProgram().add(stm);
			}
			IRStm last = block.head();
			Assert.assertTrue("Basic blocks algorithm broken? No jump at end of BB!", last.isJump());
			if (last instanceof CJUMP) { 
				// Treated as a special case because we want to "flip" branches to 
				// ensure it is followed by a false label.
				CJUMP cjump = (CJUMP) last;
				Label falseTarget = cjump.getFalseLabel();
				Label trueTarget = cjump.getTrueLabel();
				if (table.containsKey(falseTarget)) {
					//Nice! We can add the false block after the CJUMP!
					getProgram().add(cjump);
					trace(falseTarget);
				}
				else if (table.containsKey(trueTarget)) {
					getProgram().add(cjump.flip());
					trace(trueTarget);
				}
				else {
					//Shucks: both the true and false target were already
					//added to the program. 
					//=> worst case: we must insert an additional JUMP!
					Label newFalseLabel = Label.gen();
					getProgram().add(cjump.changeFalseLabel(newFalseLabel));
					getProgram().add(LABEL(newFalseLabel));
					getProgram().add(JUMP(falseTarget));
					//trace(falseTarget); // Actually not needed: it will just return immediately
				}
			}
			else { // Regular jump 
				List<Label> targets = last.getJumpTargets();
				Label target = targets.head();
				if (targets.size()!=1) {
					// JUMP with dynamic targets should never be dropped! 
					program.add(last);
				}
				else { // JUMP(NAME(target))
					if (table.containsKey(target) 
					|| target==doneLabel && table.isEmpty()) {
						//Drop this jump
					}
					else {
						program.add(last);
					}
				}
				trace(target); // continue the trace at the target of the jump
			}
		}
	}

	private Label getLabel(List<IRStm> block) {
		return ((LABEL)block.head()).getLabel();
	}     
	
	/**
	 * This method performs an integrity check. It checks that this code is
	 * "ok" w.r.t to the following:
	 *   - no JUMPS to the next label.
	 *   - all CJUMPS followed immediately by their false label. 
	 *   - the program must end with the special doneLabel (because subsequent compiler
	 *     phases rely on it to add more code at the end of the method body.
	 * @param doneLabel 
	 */
	private void verify(List<IRStm> program, Label doneLabel) {
		
		for (IRStm s=program.head(); !program.isEmpty(); program = program.tail()) {
			if (s instanceof CJUMP) {
				CJUMP cjump = (CJUMP) s;
				Label next = ((LABEL)program.get(1)).getLabel();
				Assert.assertEquals(cjump.getFalseLabel(), next);
			}
			else if (s instanceof JUMP) {
				JUMP jump = (JUMP) s;
				List<Label> labels = jump.getJumpTargets();
				Assert.assertTrue(labels.size()>=1); 
				if (labels.size()==1 && !program.tail().isEmpty()) {
					Label forbiddenLabel = labels.head();
					Label nextLabel = ((LABEL)program.get(1)).getLabel();
					Assert.assertFalse(nextLabel.equals(forbiddenLabel));
				}
			}
		}
		Assert.assertEquals(doneLabel, ((LABEL)program.getLast()).getLabel());
	}

	public List<IRStm> getProgram() {
		return program;
	}

}


