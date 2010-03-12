package minijava.codegen.assem;

import minijava.ir.temp.Color;
import minijava.ir.temp.Label;
import minijava.ir.temp.Temp;
import minijava.util.DefaultIndentable;
import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * Represents an assembly language instruction.
 * <p>
 * The assem string is a kind of template for the printed representation
 * of the instruction where the following syntax is used:
 * <ul>
 *   <li> `d<digit>  : insert destination operand here (i.e. def().get(<digit>) )
 *   <li> `s<digit>  : insert source operand here (i.e. use().get(<digit>) )
 *   <li> `j<digit>  : insert jump target here (i.e. jumps().get(<digit>) )
 *   <li> ``         : insert a ` here.
 * </ul>
 */
public abstract class Instr extends DefaultIndentable {

	private String assem;

	protected Instr(String assem) {
		this.assem = assem;
	}

	/**
	 * A Temp should be in the use list if the execution of this instruction
	 * depends on the contents of the register (i.e. the instruction reads the
	 * contents).
	 */
	public abstract List<Temp> use();

	/**
	 * A Temp should be in the def list if the execution of this instruction
	 * modifies the contents of the register (i.e. the instruction writes into
	 * the register).
	 */
	public abstract List<Temp> def();

	/**
	 * If an instruction is a jump then it must return a list of possible
	 * targets (including the next instruction if this instruction "falls
	 * through")
	 * <p>
	 * If it is not a jump (and hence always falls through to the next instruction)
	 * it should return null).
	 */
	public abstract List<Label> jumps();

	public String format() {
		List<Temp> dst = def();
		List<Temp> src = use();
		List<Label> jump = jumps();
		StringBuffer s = new StringBuffer();
		int len = assem.length();
		for(int i=0; i<len; i++)
			if (assem.charAt(i)=='`')
				switch(assem.charAt(++i)) {
				case 's': {int n = Character.digit(assem.charAt(++i),10);
				s.append(tmp(src.get(n)));
				}
				break;
				case 'd': {int n = Character.digit(assem.charAt(++i),10);
				s.append(tmp(dst.get(n)));
				}
				break;
				case 'j': {int n = Character.digit(assem.charAt(++i),10);
				s.append(jump.get(n).toString());
				}
				break;
				case '`': s.append('`'); 
				break;
				default: throw new Error("bad Assem format");
				}
			else s.append(assem.charAt(i));

		return s.toString();
	}

	/**
	 * Format a temporary. Use it's coloring if available to determine the
	 * actual register instead of the temp's original name. 
	 * <p>
	 * Note: coloring is related to register allocation. Ignore this code
	 * while on the code generation phase.
	 */
	protected String tmp(Temp temp) {
		Color color = temp.getColor();
		String name = temp.getName();
		if (color!=null) {
			if (color.toString().equals(name)) 
				return name;
			else
				return color+"/*"+temp.getName()+"*/";
		}
		else return name;
 	}

	@Override
	public void dump(IndentingWriter out) {
		out.print(this.format());
	}

}
