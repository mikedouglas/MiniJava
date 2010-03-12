package minijava.codegen.patterns;

import java.util.HashMap;
import java.util.Map;

import junit.framework.Assert;

/**
 * Whenever a wildcard pattern matches a particular node, 
 * an map entry associating the wildcard to its “value” is 
 * added to a map. This map is returned as the result of a
 * successful matching operation. 
 */
public class Matched {
	
	private Map<Wildcard<?>,Object> map = new HashMap<Wildcard<?>, Object>();

	public <T> void put(Wildcard<T> pat, T matched) {
		Assert.assertFalse(map.containsKey(pat));
		map.put(pat, matched);
	}
	
	@SuppressWarnings("unchecked")
	public <T> T get(Pat<T> pat) {
		return (T) map.get(pat);
	}

}
