package minijava.codegen.patterns;

import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * A pattern that matches anything (like a "wildcard")
 */
public class Wildcard<N> extends Pat<N> {
	@Override
	public void dump(IndentingWriter out) {
		out.print("*");
	}

	@Override
	public int size() {
		return 0;
	}

	@Override
	public void match(N toMatch, Matched matched) throws Failed {
		matched.put(this, toMatch);
	}

	@Override
	public Pat<N> build(List<Pat<?>> children) {
		return this;
	}

	@Override
	public List<Pat<?>> children() {
		return List.empty();
	}
}