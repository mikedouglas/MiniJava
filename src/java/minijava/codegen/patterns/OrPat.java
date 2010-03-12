package minijava.codegen.patterns;

import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * An or pattern is a composition of a number of alternate patterns.
 * An or patterns matches a given element if either one of its 
 * sub-patterns matches the element.
 * <p>
 * Or patterns can not be directly matched. Instead they need to be
 * exploded into a list of more primitive patterns.
 */
public class OrPat<N> extends Pat<N> {

	private List<Pat<N>> pats;

	public OrPat(Pat<N>... pats) {
		this.pats = List.list(pats);
	}

	@Override
	public void match(N toMatch, Matched matched)
			throws minijava.codegen.patterns.Pat.Failed, ClassCastException {
		//The way we currently use these patterns, the match operation will not
		//be called unless a pattern has a fixed size.
		//It may be useful in the future to implement this method however.
		//Note: but this will be tricky to implement withoiut changing the
		//  signature of the match method (will need to be able to undo
		//  effects of matching one "or branch" before proceeding to the next one.
		throw new UnsupportedOperationException();
	}

	@Override
	public int size() {
		//Note: we could implement this safely/correctly if all
		//sub patterns have the same size.
		throw new UnsupportedOperationException();
	}

	@Override
	public void dump(IndentingWriter out) {
		out.println("(");
		out.print(")");
	}

	@Override
	public List<Pat<N>> explode() {
		return pats;
	}

	@Override
	public Pat<N> build(List<Pat<?>> children) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<Pat<?>> children() {
		throw new UnsupportedOperationException();
	}
	
}
