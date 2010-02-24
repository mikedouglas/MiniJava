package minijava.ir.tree;

//import minijava.ir.interp.Int;
import minijava.ir.temp.Label;
import minijava.ir.temp.Temp;
import minijava.ir.tree.BINOP.Op;
import minijava.ir.tree.CJUMP.RelOp;
import minijava.util.List;

/**
 * This class has a bunch of static helper methods here to
 * create IR nodes or IR code fragments. 
 * <p>
 * These helper methods make it much easier to write readable
 * IR code generators (Hint: use import static minijava.ir.tree.IR.*
 * to import these helper methods so you do not have to prepend
 * every call with "IR.".
 */
public class IR {
	
	public static final IRExp TRUE = CONST(1);
	public static final IRExp FALSE = CONST(0);
	
	/**
	 * A statement that doesn't do anything:
	 */
	public static final IRStm NOP = EXP(CONST(0));

	public static IRExp BINOP(Op op, IRExp l, IRExp r) {
		BINOP result = new BINOP(op, l, r);
/*		FIXME: disabled interp
  		if (l instanceof CONST && r instanceof CONST) {
			Int value = (Int) result.interp(null);
			return CONST(value.value);
		}
*/
		return result;
	}

	public static CALL CALL(Label lab, IRExp... args) {
		return new CALL(NAME(lab), List.list(args));
	}
	
	public static IRExp CALL(Label lab, List<IRExp> args) {
		return new CALL(NAME(lab), args);
	}

	public static IRStm CJUMP(RelOp compare, IRExp l, IRExp r, Label t, Label f ) {
		return new CJUMP(compare, l, r, t, f);
	}

	public static IRExp CONST(int v) {
		return new CONST(v);
	}

	public static IRExp ESEQ(IRStm s, IRExp e) {
		if (s == NOP) return e;
		return new ESEQ(s,e);
	}

	public static IRStm EXP(IRExp exp) {
		return new EXP(exp);
	}

	public static IRStm JUMP(Label target) {
		return new JUMP(target);
	}
	public static LABEL LABEL(Label label) {
		return new LABEL(label);
	}

	public static MEM MEM(IRExp e) { return new MEM(e); }

	public static IRExp MINUS(IRExp l, int r) {
		return BINOP(Op.MINUS, l, CONST(r));
	}

	public static IRStm MOVE(IRExp d, IRExp s) {
		return new MOVE(d,s);
	}

	public static IRStm MOVE(Temp d, IRExp s) {
		return new MOVE(TEMP(d),s);
	}
	public static IRExp MUL(IRExp l, int r) {
		return MUL(l,CONST(r));
	}

	private static IRExp MUL(IRExp l, IRExp r) {
// The following have been commented out.
// They are not really safe optimisations, because they
// may throw away side effects!
//		if (l.isCONST(0)) return l;
//		if (r.isCONST(0)) return r;
		if (l.isCONST(1)) return r;
		if (r.isCONST(1)) return l;
		return BINOP(Op.MUL, l, r);
	}

	private static IRExp NAME(Label lab) {
		return new NAME(lab);
	}

	public static IRExp PLUS(IRExp v, int offset) {
		return PLUS(v, CONST(offset));
	}

	public static IRExp PLUS(IRExp l, IRExp r) {
		if (l.isCONST(0))
			return r;
		if (r.isCONST(0))
			return l;
		else
			return BINOP(Op.PLUS, l, r);
	}
 
	public static IRStm SEQ(IRStm... stms) {
		IRStm s = NOP;
		for (int i = stms.length-1; i >= 0; i--) {
			if (s==NOP) 
				s = stms[i];
			else if (stms[i]==NOP) {
				//skip
			}
			else {
				s = new SEQ(stms[i],s);
			}
		}
		return s;
	}

	public static TEMP TEMP(Temp name) {
		return new TEMP(name);
	}

}
