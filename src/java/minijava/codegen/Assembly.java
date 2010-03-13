package minijava.codegen;

import java.util.Iterator;

import minijava.util.DefaultIndentable;
import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * Represents the result of applying a code generator to a list of IR fragments.
 */
public class Assembly extends DefaultIndentable implements Iterable<AssemFragment> {

	private List<AssemFragment> fragments = List.empty();
	
	@Override
	public void dump(IndentingWriter out) {
		out.indent();
//		if (sourceFile!=null)
//			out.println(".file "+sourceFile);
		out.println(".text");
		out.outdent();
		
		for (AssemFragment f : fragments) {
			out.println(f);
		}
		
		out.indent();
		
		out.println(".ident	\"minijavac: cs411 course project 2008W2\"");
		out.println(".section	.note.GNU-stack,\"\",@progbits");
		out.outdent();
	}

	public void add(AssemFragment code) {
		fragments.add(code);
	}

	@Override
	public Iterator<AssemFragment> iterator() {
		return fragments.iterator();
	}

}
