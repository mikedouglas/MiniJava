package minijava.ir.interp;

/**
 * There is a choice of three different methods for setting up the IR simulation
 * of methods/procedures.
 */
public enum InterpMode {

	LINEARIZED_IR,	// Simulate execution with linearized IR
	BASIC_BLOCKS,	// Simulate execution with basic blocks
	TRACE_SCHEDULE	// Simulate execution with the code produced by Trace Scheduling
	
}
