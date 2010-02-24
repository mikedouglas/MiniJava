package minijava.test.translate;

import junit.framework.Assert;
import minijava.ir.frame.Access;
import minijava.ir.frame.Frame;
import minijava.ir.frame.x86.X86Frame;
import minijava.ir.temp.Label;
import minijava.util.List;

import org.junit.Test;

/**
 * This test is not very thorough. It basically only checks whether allocated locals
 * formals adresses are distinct from one another.
 * <p>
 * You probably want to craft some tests that are more specific and check whether
 * the *right* adresses get allocated for locals and formals (which depends on
 * understanding the specific calling conventions of the x86 architecture).
 * 
 * @author kdvolder
 */
public class TestX86Frame {
	
	private Frame factory =
		//This should be the only reference to X86Frame. Everything else
		//should use Frame methods only
		X86Frame.factory; 
	
	@Test
	public void noFormals() throws Exception {
		System.out.println("Allocating 0 formals and 4 locals");
		Label empty = Label.generate("empty");
		Frame frame = factory.newFrame(empty, List.list(new Boolean[0]));
		Assert.assertEquals(0, frame.getFormals().size());
		Access[] var = new Access[4];
		for (int i = 0; i < var.length; i++) {
			boolean escapes = i%2==0;
			var[i] = frame.allocLocal(escapes);
			System.out.println("var["+i+"] = "+var[i]+"\tescapes = "+escapes);
		}
		// This should have allocated 4 distinct locations. (Two of
		// them inFrame and two of them inRegisters).
		for (int i = 0; i < var.length; i++) {
			for (int j = 0; j < var.length; j++) {
				Assert.assertEquals(i==j, var[i].equals(var[j]));
				Assert.assertEquals(i==j, var[i].toString().equals(var[j].toString()));
			}
		}
	}

	@Test
	public void severalFormalsFrame() throws Exception {
		System.out.println("Allocating 4 formals and 4 locals");
		Label lab = Label.generate("four");
		List<Boolean> formalsEscape = List.list(true,false,true,false); 
		Frame frame = factory.newFrame(lab, List.list(true,false,true,false));
		List<Access> formals = frame.getFormals();
		
		Assert.assertEquals(formalsEscape.size(), formals.size());
		
		Access[] var = new Access[8];
		for (int i = 0; i < formals.size(); i++) {
			var[i] = formals.get(i);
			Boolean escapes = formalsEscape.get(i);
			System.out.println("var["+i+"] = "+var[i]+"\tescapes = "+escapes);
		}
		
		for (int i = formals.size(); i < var.length; i++) {
			//boolean escapes = i%2==0;
			boolean escapes = true; // force all locals to be inFrame for this test
			var[i] = frame.allocLocal(escapes);
			System.out.println("var["+i+"] = "+var[i]+"\tescapes = "+escapes);
		}
		// This should have allocated 8 distinct locations (we are checking here
		// that formals and inframe locals don't accidentally get mapped to the same frame
		// adresses. for this test to work properly, it is assumed there is a reasonable
		// implementation of toString that will format two Access objects representing
		// the same inFrame address as the same string. 
		for (int i = 0; i < var.length; i++) {
			for (int j = 0; j < var.length; j++) {
				Assert.assertEquals(i==j, var[i].equals(var[j]));
				Assert.assertEquals(i==j, var[i].toString().equals(var[j].toString()));
			}
		}
	}
}
