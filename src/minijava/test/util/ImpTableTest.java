package minijava.test.util;

import minijava.util.ImpTable;
import minijava.util.ImpTable.DuplicateException;

import org.junit.Assert;
import org.junit.Test;

/**
 * It is a good idea to provide unit tests for all non-trivial 
 * data structure implementations. This class contains tests for
 * our implementation of Symbol tables provided in the class
 * minijava.table.ImpTable.
 * <p>
 * The implementation of Table that is provided is already complete.
 * These tests should run and pass "out of the box".
 * 
 * @author kdvolder
 */
public class ImpTableTest {
	
	@Test public void testEmptyTable() {
		ImpTable<Integer> tab = new ImpTable<Integer>();
		Assert.assertTrue(tab.isEmpty());
		Assert.assertNull(tab.lookup("something"));
	}

	@Test public void testOneInsert() throws DuplicateException {
		ImpTable<Integer> tab = new ImpTable<Integer>();
		tab.put("x", 10);
		Assert.assertFalse(tab.isEmpty());
		Integer found = tab.lookup("x");
		Assert.assertNotNull(found);
		Assert.assertEquals(10, (int)found);
	}

	@Test public void testMultipleInsert() throws DuplicateException {
		ImpTable<Integer> tab = new ImpTable<Integer>();
		tab.put("x", 10);
		tab.put("y", 20);
		tab.put("z", 30);
		Assert.assertFalse(tab.isEmpty());
		
		Assert.assertEquals(10, (int)tab.lookup("x"));
		Assert.assertEquals(20, (int)tab.lookup("y"));
		Assert.assertEquals(30, (int)tab.lookup("z"));
		
		Assert.assertNull(tab.lookup("X"));
	}
	
	@Test public void testMultipleInsertOfSame() throws DuplicateException {
		ImpTable<Integer> tab = new ImpTable<Integer>();
		tab.put("x", 10);
		tab.put("y", 20);
		try {
			tab.put("x", 30);
			Assert.fail("Should throw a duplicate excpetion");
		} catch (DuplicateException e) {
		}
		Assert.assertFalse(tab.isEmpty());
		
		Assert.assertEquals(10, (int)tab.lookup("x")); // duplicate was not added!
		Assert.assertEquals(20, (int)tab.lookup("y"));
	}
	
}
