package minijava.codegen.muncher;

import static minijava.util.List.cons;
import static minijava.util.List.list;

import java.util.HashSet;
import java.util.Set;

import minijava.codegen.patterns.Matched;
import minijava.ir.tree.IRNode;
import minijava.util.DefaultIndentable;
import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * An instance of this class maintains a table of {@link MunchRule} instances. 
 * <p>
 * It can be used to find and trigger a matching MunchRule for a given IRNode.
 * <p>
 * After filling the table with MunchRules an instance of this class does not hold
 * any state during the munching process. So you do not need to create a new Table of
 * MunchRules each time you want to use it (it is advisable to only build one instance
 * for IRStm and one for IRExp and then to keep reusing those instances, because building
 * the rules table and sorting it according to priority costs time).
 * 
 * @author kdvolder
 */
public class MuncherRules<N extends IRNode, R> extends DefaultIndentable {
	
	/**
	 * These are the actual rules, sorted based from highest priority to lowest.
	 */
	private List<MunchRule<N, R>> rules = List.empty();
	
	/**
	 * This is a set of rule signatures. Only rules with a signature different from the
	 * rules already present will be added.
	 */
	private Set<String> ruleSignatures = new HashSet<String>();

	R munch(Muncher muncher, N node) {
		//This selection / trigger mechanism is a naive linear scan of all the rules. In "real" 
		//compilers a more sophisticated algorithm would be used to speed up the matching process.
		for (MunchRule<N, R> rule : rules) {
			Matched match = rule.match(node);
			if (match!=null) {
				return rule.trigger(muncher, match);
			}
		}
		throw new Error("No matching munch rule for this node:\n"+node);
	}

	public void add(MunchRule<N, R> _munchRule) {
		if (_munchRule.isAtomic()) {
			if (!ruleSignatures.contains(_munchRule.signature())) {
				rules = insert(_munchRule, rules);
				ruleSignatures.add(_munchRule.signature());
			}
		}
		else 
			for (MunchRule<N, R> munchRule : _munchRule.explode())
				this.add(munchRule);
	}

	@SuppressWarnings("unchecked")
	private List<MunchRule<N, R>> insert(MunchRule<N, R> munchRule,
			List<MunchRule<N, R>> rules) {
		if (rules.isEmpty())
			return list(munchRule);
		else {
			MunchRule<N, R> first = rules.head();
			if (first.getPriority()<=munchRule.getPriority())
				return cons(munchRule, rules);
			else
				return cons(first, insert(munchRule, rules.tail()));
		}
	}

	@Override
	public void dump(IndentingWriter out) {
		out.print("MuncherRules");
		out.print(rules);
	}

}
