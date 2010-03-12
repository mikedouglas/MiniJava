package minijava.codegen.muncher;

import minijava.codegen.patterns.Matched;
import minijava.codegen.patterns.Pat;
import minijava.ir.tree.IRNode;
import minijava.util.DefaultIndentable;
import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * Create a Catalog of munch rules and add them to your Muncher.
 * <p>
 * Each much rule consists of pattern to match against an IR node
 * and trigger method that should emit instructions and call the muncher
 * recursively to munch the children. 
 */
public abstract class MunchRule<N extends IRNode, R> extends DefaultIndentable {
	
	private static final class ExplodedRule<N extends IRNode, R> extends MunchRule<N, R> {
		private MunchRule<N, R> trigger;

		private ExplodedRule(Pat<N> pat, MunchRule<N, R> trigger) {
			super(pat);
			this.trigger = trigger;
		}

		@Override
		protected R trigger(Muncher m, Matched match) {
			return trigger.trigger(m, match);
		}
	}

	public MunchRule(Pat<N> pat) {
		this.pat = pat;
	}
	private Pat<N> pat;
	private int matched = 0;
	
	/**
	 * This method is called when a rules gets triggered (because
	 * it's pattern is matched).
	 * <p>
	 * @param match: contains a map from parts of the pattern
	 * to the IR fragments that these sub patterns matched.
	 * @param m  a use this muncher to munch the "children" of a matched pattern.
	 */
	protected abstract R trigger(Muncher m, Matched match);
	
	public Matched match(N to) {
		Matched result = pat.tryMatch(to);
		if (result!=null) matched++;
		return result;
	}

	public int getPriority() {
		return pat.size();
	}
	
	@Override
	public void dump(IndentingWriter out) {
		out.print(pat);
		out.print(" ==> "+matched+" times");
	}

	@SuppressWarnings("unchecked")
	public List<MunchRule<N, R>> explode() {
		if (isAtomic())
			return List.list(this);
		else {
			List<Pat<N>> patterns = pat.explode();
			List<MunchRule<N, R>> result = List.empty();
			for (Pat<N> pat : patterns) {
				result.add(new ExplodedRule<N, R>(pat, this));
			}
			return result;
		}
	}

	/**
	 * An "atomic" rule is a rule that will be exploded into a just
	 * singleton list containing only the rule itself.
	 */
	public boolean isAtomic() {
		return pat.isFixedSize();
	}

	/**
	 * Rules with identical signatures will match exactly the same patterns.
	 * (Thus if two rules with identical signature are added to list of MunchRules
	 * only one of them can ever actually be triggered.
	 * <p>
	 * We use the signature to remove redundant rules.
	 */
	public String signature() {
		return pat.toString();
	}
}