package minijava.ir.canon;

//import minijava.ir.interp.X86SimFrame;
import minijava.ir.temp.Label;
import minijava.ir.temp.Temp;
import minijava.ir.tree.CALL;
import minijava.ir.tree.CONST;
import minijava.ir.tree.ESEQ;
import minijava.ir.tree.EXP;
import minijava.ir.tree.IRExp;
import minijava.ir.tree.IRStm;
import minijava.ir.tree.MOVE;
import minijava.ir.tree.NAME;
import minijava.ir.tree.SEQ;
import minijava.ir.tree.TEMP;
import minijava.util.IndentingWriter;
import minijava.util.List;

import static minijava.ir.tree.IR.*;

/*
 * Kris De Volder:
 * 
 * This code was taken verbatim from the text book's site:
 * 
 * http://www.cambridge.org/resources/052182060X/ 
 * 
 * I only  made some minor modifications to make this code use 
 * List<IRStm> and List<IRExp> rather than use the book's 
 * linked lists.
 */

class MoveCall extends IRStm {
	final TEMP dst;
	final CALL src;
	MoveCall(TEMP d, CALL s) {dst=d; src=s;}
	
	@Override
	public List<IRExp> kids() {return src.kids();}
	
	@Override
	public IRStm build(List<IRExp> kids) {
		return new MOVE(dst, src.build(kids));
	}
	@Override
	public void dump(IndentingWriter out) {
		MOVE(dst, src).dump(out);
	}
/*
	@Override
	public Label interp(X86SimFrame env) {
		// No need to implement this. This is a temporary class
		// its instances will be replaced / expanded by the end of
		// the canonicalisation process.
		throw new Error("Not implemented");
	}
*/
}   

class ExpCall extends IRStm {
	final CALL call;
	ExpCall(CALL c) {call=c;}
	public List<IRExp> kids() {return call.kids();}
	public IRStm build(List<IRExp> kids) {
		return new EXP(call.build(kids));
	}
	@Override
	public void dump(IndentingWriter out) {
		EXP(call).dump(out);
	}
/*
	@Override
	public Label interp(X86SimFrame env) {
		// No need to implement this. This is a temporary class ...
		throw new Error("Not implemented");
	}
*/
}   

class StmExpList {
	IRStm stm;
	List<IRExp> exps;
	StmExpList(IRStm s, List<IRExp> e) {stm=s; exps=e;}
}

public class Canon {

	private static final List<IRStm> nullStmList = List.empty();
	private static final List<IRExp> nullExpList = List.empty();

	static boolean isNop(IRStm a) {
		return a instanceof EXP
		&& ((EXP)a).exp instanceof CONST;
	}

	static IRStm seq(IRStm a, IRStm b) {
		if (isNop(a)) return b;
		else if (isNop(b)) return a;
		else return new SEQ(a,b);
	}

	static boolean commute(IRStm a, IRExp b) {
		return isNop(a)
		|| b instanceof NAME
		|| b instanceof CONST;
	}

	static IRStm do_stm(SEQ s) { 
		return seq(do_stm(s.left), do_stm(s.right));
	}

	static IRStm do_stm(MOVE s) { 
		if (s.dst instanceof TEMP 
				&& s.src instanceof CALL) 
			return reorder_stm(new MoveCall((TEMP)s.dst,
					(CALL)s.src));
		else if (s.dst instanceof ESEQ)
			return do_stm(new SEQ(((ESEQ)s.dst).stm,
					new MOVE(((ESEQ)s.dst).exp,
							s.src)));
		else return reorder_stm(s);
	}

	static IRStm do_stm(EXP s) { 
		if (s.exp instanceof CALL)
			return reorder_stm(new ExpCall((CALL)s.exp));
		else return reorder_stm(s);
	}

	static IRStm do_stm(IRStm s) {
		if (s instanceof SEQ) return do_stm((SEQ)s);
		else if (s instanceof MOVE) return do_stm((MOVE)s);
		else if (s instanceof EXP) return do_stm((EXP)s);
		else return reorder_stm(s);
	}

	static IRStm reorder_stm(IRStm s) {
		StmExpList x = reorder(s.kids());
		return seq(x.stm, s.build(x.exps));
	}

	static ESEQ do_exp(ESEQ e) {
		IRStm stms = do_stm(e.stm);
		ESEQ b = do_exp(e.exp);
		return new ESEQ(seq(stms,b.stm), b.exp);
	}

	static ESEQ do_exp (IRExp e) {
		if (e instanceof ESEQ) return do_exp((ESEQ)e);
		else return reorder_exp(e);
	}

	static ESEQ reorder_exp (IRExp e) {
		StmExpList x = reorder(e.kids());
		return new ESEQ(x.stm, e.build(x.exps));
	}

	static StmExpList nopNull = new StmExpList(new EXP(new CONST(0)),nullExpList);

	static StmExpList reorder(List<IRExp> exps) {
		if (exps.isEmpty()) return nopNull;
		else {
			IRExp a = exps.head();
			if (a instanceof CALL) {
				Temp t = new Temp();
				IRExp e = new ESEQ(new MOVE(new TEMP(t), a),
						new TEMP(t));
				return reorder(List.cons(e, exps.tail()));
			} else {
				ESEQ aa = do_exp(a);
				StmExpList bb = reorder(exps.tail());
				if (commute(bb.stm, aa.exp))
					return new StmExpList(seq(aa.stm,bb.stm), 
							List.cons(aa.exp,bb.exps));
				else {
					Temp t = new Temp();
					return new StmExpList(
							seq(aa.stm, 
									seq(new MOVE(new TEMP(t),aa.exp),
											bb.stm)),
											List.cons(new TEMP(t), bb.exps));
				}
			}
		}
	}

	static List<IRStm> linear(SEQ s, List<IRStm> l) {
		return linear(s.left,linear(s.right,l));
	}
	static List<IRStm> linear(IRStm s, List<IRStm> l) {
		if (s instanceof SEQ) return linear((SEQ)s, l);
		else return List.cons(s,l);
	}

	static public List<IRStm> linearize(IRStm s) {
		return linear(do_stm(s), nullStmList);
	}
}
