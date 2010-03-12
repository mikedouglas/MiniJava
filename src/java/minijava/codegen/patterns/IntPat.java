package minijava.codegen.patterns;

import minijava.util.IndentingWriter;
import minijava.util.List;

public class IntPat extends Pat<Integer> {
	
	private int value;

	public IntPat(int value) {
		this.value = value;
	}

	@Override
	public Pat<Integer> build(List<Pat<?>> children) {
		return this;
	}

	@Override
	public List<Pat<?>> children() {
		return List.empty();
	}

	@Override
	public void match(Integer toMatch, Matched matched)
			throws Failed, ClassCastException {
		if (toMatch!=value) fail();
	}

	@Override
	public void dump(IndentingWriter out) {
		out.print(value);
	}

}
