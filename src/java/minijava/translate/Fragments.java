package minijava.translate;

import java.util.Iterator;

import minijava.ir.frame.Frame;
import minijava.util.DefaultIndentable;
import minijava.util.IndentingWriter;
import minijava.util.List;

/**
 * A collection of fragments of IR code (in minijava without inheritance,
 * there are only method fragments. In other languages various data
 * fragments may exist as well. 
 * <p>
 * E.g. if inheritance is implemented in MiniJava we will likely need to
 * have data fragments to store virtual method tables computed by the
 * compiler. If String data type is supported, we will need String data
 * fragments to represent String literals.
 */
public class Fragments extends DefaultIndentable implements Iterable<Fragment> {
	
	/**
	 * The translator should provide its FrameFactory in here. This object pretty much
	 * centralises/isolates architecture specific code. Subsequent phases should use the
	 * same target architecture as the translator, so it is convenient to have the 
	 * translator pass along this Frame factory object along with its generated IR code.
	 */
	private Frame frameFactory;
	
	/**
	 * Generate IR code (and possibly other Fragments):
	 */
	private List<Fragment> frags = List.empty();

	public Fragments(Frame frameFactory) {
		super();
		this.frameFactory = frameFactory;
	}

	@Override
	public void dump(IndentingWriter out) {
		out.println("Fragments {");
		out.indent();
		
		for (Fragment frag : frags) {
			out.println(frag);
		}
		
		out.outdent();
		out.print("}");
	}

	public void add(Fragment frag) {
		frags.add(frag);
	}

	/**
	 * To allow writing "foreach" loop on Fragments.
	 */
	@Override
	public Iterator<Fragment> iterator() {
		return frags.iterator();
	}

	/**
	 * Fetch the (target architecture specific) frame factory that was used to 
	 * produce this IR. 
	 */
	public Frame getFrameFactory() {
		return frameFactory;
	}

}
