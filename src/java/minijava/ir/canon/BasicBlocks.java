package minijava.ir.canon;

import static minijava.ir.tree.IR.JUMP;
import static minijava.ir.tree.IR.LABEL;
import junit.framework.Assert;
import minijava.ir.temp.Label;
import minijava.ir.tree.IRStm;
import minijava.ir.tree.LABEL;
import minijava.util.List;

public class BasicBlocks {

	/**
	 * If the IR code didn't end with an explicit JUMP, we add one that jumps
	 * to this generated done label. If the code did end with an explicit jump, then we
	 * don't add a done label and this variable remains set to null.
	 */
	public Label doneLabel = null;
	
	/**
	 * We collect the basic blocks in this list. You can retrieve them from here.
	 */
	public List<List<IRStm>> blocks = List.empty();

	private List<IRStm> currentBlock = null;

	private void startNewBlock(LABEL startLabel) {
		if (currentBlock!=null) {
			// If the current block was not ended, it must mean it "falls through" without
			// an explicit jump.
			currentBlock.add(JUMP(startLabel.getLabel()));
			endCurrentBlock();
		}
		currentBlock = List.list((IRStm)startLabel);
	}
	
	private void endCurrentBlock() {
		Assert.assertNotNull("There is no current block", currentBlock);
		blocks.add(currentBlock);
		currentBlock = null;
	}

	private void mkBlocks(List<IRStm> l) {
		if (!(l.head() instanceof LABEL)) 
			l = List.cons(LABEL(Label.gen()), l);
		
		for (IRStm stm : l) {
			if (stm.isJump()) {
				currentBlock.add(stm);
				endCurrentBlock();
			}
			else if (stm instanceof LABEL) {
				startNewBlock((LABEL)stm);
			}
			else
				currentBlock.add(stm);
		}
		if (currentBlock!=null) {
			//The code "fell of the end" without a JUMP.
			doneLabel = Label.generate("DONE");
			currentBlock.add(JUMP(doneLabel));
			endCurrentBlock();
		}
	}

	public BasicBlocks(List<IRStm> stms) {
		mkBlocks(stms);
	}
}
