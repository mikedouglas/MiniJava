package minijava.test.translate;

import minijava.ir.interp.InterpMode;

/**
 * This runs all the same tests as the TestTranslate and TestBasicBlocks test suite.
 * <p>
 * The difference is that the IR from method bodies is converted to
 * basic blocks rather than just to linearised IR.
 * <p>
 * Presumably, if your IR code works correctly in TestTranslate/TestBasicBlocks but
 * there are problems in this test, then it probably means the TraceScheduling
 * implementation something wrong (so this is more likely a bug in
 * my TraceScheduling code than in your IR translation code :-)
 * 
 * @author kdvolder
 */
public class TestTraceSchedule extends TestTranslate {

	@Override
	protected InterpMode getSimulationMode() {
		return InterpMode.TRACE_SCHEDULE;
	}
}
