package minijava.ir.tree;

import org.junit.Assert;

//import minijava.ir.interp.Word;
//import minijava.ir.interp.X86SimFrame;
import minijava.ir.temp.Color;
import minijava.ir.temp.Temp;
import minijava.util.IndentingWriter;
import minijava.util.List;

public class TEMP extends IRExp {
	public final Temp temp;
	public TEMP(Temp t) {
		Assert.assertNotNull(t);
		temp=t;
	}
	@Override
	public void dump(IndentingWriter out) {
		out.print("TEMP ");
		out.print(temp);
		if (temp.getColor()!=null) { 
			out.print(":");
			out.print(temp.getColor());
		}
	}
	@Override
	public IRExp build(List<IRExp> kids) {
		return this;
	}
	@Override
	public List<IRExp> kids() {
		return List.empty();
	}
//	@Override
//	public Word interp(X86SimFrame env) {
//		//We only get here if we are reading the TEMP.
//		//A TMP in the dst of a MOVE is treated explicitly by MOVE
//		return env.getTemp(temp);
//	}
	public Color getColor() {
		return temp.getColor();
	}
	
//	@Override
//	public void set(Word value, X86SimFrame env) {
//		env.setTemp(temp, value);
//	}
}

