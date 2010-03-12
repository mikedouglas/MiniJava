package minijava.codegen.patterns;

import minijava.ir.temp.Label;
import minijava.ir.temp.Temp;
import minijava.ir.tree.IRExp;
import minijava.ir.tree.IRStm;
import minijava.ir.tree.BINOP.Op;
import minijava.ir.tree.CJUMP.RelOp;
import minijava.util.List;

/**
 * This class provides a few useful static methods to build
 * IR patterns.
 * 
 * @author kdvolder
 */
@SuppressWarnings("unchecked")
public class IRPat {
	
	// Stm patterns:
	public static Pat<IRStm> JUMP(Pat<IRExp> exp) { return new JUMPPat(exp); }
	public static Pat<IRStm> MOVE(Pat<IRExp> dst, Pat<IRExp> src) {
		return new MOVEPat(dst, src);
	}
	public static Pat<IRStm> EXP(Pat<IRExp> exp) { return new EXPPat(exp); }
	public static Pat<IRStm> CJUMP(Pat<RelOp> op, Pat<IRExp> l, Pat<IRExp> r, 
			Pat<Label> t, Pat<Label> f) {
		return new CJUMPPat(op, l, r, t, f);
	}
	public static Pat<IRStm> LABEL(Pat<Label> l) { return new LABELPat(l); }

	// Exp patterns:
	public static BINOPPat BINOP(Op op, Pat<IRExp> l, Pat<IRExp> r) {
		return new BINOPPat(op, l, r);
	}
	public static Pat<IRExp> PLUS(Pat<IRExp> l, Pat<IRExp> r) {
		return or( BINOP(Op.PLUS, l, r), 
				   BINOP(Op.PLUS, r, l)  );
	}
	public static Pat<IRExp> PLUS(Pat<IRExp> x, Pat<IRExp> y, Pat<IRExp> z) {
		return or( PLUS( x, PLUS(y, z) ), 
				   PLUS( PLUS(x, y), z) );
	}
	public static Pat<IRExp> MUL(Pat<IRExp> l, Pat<IRExp> r) {
		return or( BINOP(Op.MUL, l, r),
				   BINOP(Op.MUL, r, l) );
	}
	public static Pat<IRExp> MINUS(Pat<IRExp> l, Pat<IRExp> r) {
		return BINOP(Op.MINUS, l, r);
	}
	public static Pat<IRExp> CALL(Pat<IRExp> proc, Pat<List<IRExp>> args) {
		return new CALLPat(proc, args);
	}
	public static Pat<IRExp> MEM(Pat<IRExp> exp) { return new MEMPat(exp); }
	public static Pat<IRExp> TEMP(Pat<Temp> t) { return new TEMPPat(t); }
	public static Pat<IRExp> NAME(Pat<Label> l) { return new NAMEPat(l); }
	public static Pat<IRExp> CONST(Pat<Integer> v) { return new CONSTPat(v); }
	public static Pat<IRExp> CONST(int i) { return CONST(new IntPat(i)); }

	// Special:
	public static <N> Pat<N> or(Pat<N>... pats) {
		return new OrPat(pats);
	}

	
}
