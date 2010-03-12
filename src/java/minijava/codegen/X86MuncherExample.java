package minijava.codegen;

import static minijava.codegen.patterns.IRPat.CONST;
import static minijava.codegen.patterns.IRPat.MOVE;
import static minijava.codegen.patterns.IRPat.PLUS;
import static minijava.codegen.patterns.IRPat.TEMP;
import static minijava.util.List.list;
import minijava.codegen.assem.A_MOVE;
import minijava.codegen.assem.A_OPER;
import minijava.codegen.assem.Instr;
import minijava.codegen.muncher.MunchRule;
import minijava.codegen.muncher.Muncher;
import minijava.codegen.muncher.MuncherRules;
import minijava.codegen.patterns.Matched;
import minijava.codegen.patterns.Pat;
import minijava.ir.frame.Frame;
import minijava.ir.temp.Temp;
import minijava.ir.tree.IRExp;
import minijava.ir.tree.IRStm;
import minijava.util.List;

/**
 * This is an example of how to create a subclass of Muncher. It is a severely trimmed
 * down version of my own X86Muncher class.
 * <p>
 * The muncher rules facility is somewhat complex. This should help you figure out how to get
 * it going...
 * 
 * @author kdvolder
 */
public class X86MuncherExample extends Muncher {
	
	/**
	 * Empty list of Temps, use this  constant if you experience problems with just
	 * using List.empty() (the Java type ckecker doesn't seem to like it
	 * because it sometimes can't infer the type from its usage context.)
	 */
	private static final List<Temp> noTemps = List.empty();
	
	private static MuncherRules<IRStm, Void> sm = new MuncherRules<IRStm, Void>();
	private static MuncherRules<IRExp, Temp> em = new MuncherRules<IRExp, Temp>();
	
	public X86MuncherExample(Frame frame) {
		super(frame, sm, em);
	}
	
	//////////// The munching rules ///////////////////////////////
	
	static { //Done only once, at class loading time.
		
		// Pattern "variables" (used by the rules below)
		
		final Pat<IRExp>         _e_ = Pat.any();
		final Pat<Temp>          _t_ = Pat.any();
		final Pat<Integer>       _i_ = Pat.any();
		
		// An example of a Stm muncher rule:
		sm.add(new MunchRule<IRStm, Void>( MOVE(TEMP(_t_), _e_) ) {
			@Override
			protected Void trigger(Muncher m, Matched c) {
				m.emit(A_MOV( c.get(_t_),
						      m.munch(c.get(_e_)) ));
				return null;
			}
		});	
		
		// An example of an Exp muncher rule
		em.add(new MunchRule<IRExp, Temp>(PLUS(_e_, CONST(_i_))) {
			@Override
			protected Temp trigger(Muncher m, Matched c) {
				Temp sum = new Temp();
				m.emit( A_MOV(sum, m.munch(c.get(_e_))) );
				m.emit( A_ADD(sum, c.get(_i_)) );
				return sum;
			}
		});
		
		//TODO: You'll need to add more rules...
		
	}
	
	///////// Helper methods to generate X86 assembly instructions //////////////////////////////////////
	
	private static Instr A_ADD(Temp reg, int i) {
		return new A_OPER("addl    $"+i+", `d0", 
				list(reg),
				list(reg));
	}
	private static Instr A_MOV(Temp d, Temp s) {
		return new A_MOVE("movl    `s0, `d0", d, s);
	}

	
	/**
	 * For debugging. This shows you a representation of the actual rules in your
	 * Muncher, as well as some usage statistics (how many times each rule got triggered).
	 */
	public static void dumpRules() {
		System.out.println("StmMunchers: "+sm);
		System.out.println("ExpMunchers: "+em);
	}
}
