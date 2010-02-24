package minijava.visitor;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;

public abstract class ReflectionVisitor {
	
	/*
	 * This allows the Visitor to avoid using accept calls for visits
	 * and allows passing of any number of parameters
	 */
	@SuppressWarnings("unchecked")
	public Object visit(Object ... args) {
		if (args != null) {
			Class[] visitTypes = new Class[args.length];
			
			for (int i = 0; i < visitTypes.length; i++) {
				Class argClass = args[i].getClass();
				visitTypes[i] = argClass;
			}
						
			try {
				return this.getClass().getMethod("visit", visitTypes).invoke(this, args);
			} catch (NoSuchMethodException e) {
				// then call the visitor on fields of the class
				Object o = args[0];
				if (! ((o instanceof Number)    |
					   (o instanceof Byte)      |
					   (o instanceof Short)     |
					   (o instanceof Character) |
					   (o instanceof Boolean))) {
					Field[] fields = visitTypes[0].getFields();
					for (Field field : fields) {
						visitTypes[0] = field.getClass();
						try {
							this.getClass().getMethod("visit", visitTypes);
						} catch (SecurityException e2) {
							// shouldn't happen
						} catch (NoSuchMethodException e2) {
							continue;
						}
						try { this.visit(field); }
						catch(Exception e1) {}
					}
				}
			} catch (IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (SecurityException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				Class<? extends Throwable> excpClass = e.getCause().getClass();
				
				// This is needed to let the visit() methods throw
				// exceptions of their own, such as when the identifier
				// can't be found in the environment.
				if (excpClass.equals(new Error().getClass())) {
					throw (Error) e.getCause();
				}
				else {
					e.printStackTrace();
				}
			}
		}
		
		// shouldn't get here
		return null;
	}
}
