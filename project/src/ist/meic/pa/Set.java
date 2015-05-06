package ist.meic.pa;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class Set extends Command {

	@Override
	public void execute() throws NoSuchFieldException, SecurityException,
			ClassNotFoundException, NoSuchMethodException,
			IllegalArgumentException, IllegalAccessException,
			InvocationTargetException {

		Field classField = cls.getDeclaredField(inputArgs[1]);
		Class<?> fieldType = classField.getType();
		classField.setAccessible(true);
		String fieldName = fieldType.getName();
		fieldName = fieldName.replace("java.lang.", "");
		fieldName = fieldName.substring(0, 1).toUpperCase() + fieldName.substring(1);
		Method parseMeth;

		//in the case of some primitive types names (int / Integer) some parsing is needed
		if (fieldName.equals("Int") || fieldName.equals("Integer")) {
			fieldType = Class.forName("java.lang.Integer");
			parseMeth = fieldType.getDeclaredMethod("parseInt", String.class);
			classField.set(passedObj, parseMeth.invoke(fieldType, inputArgs[2]));
			
		} else if(fieldName.equals("Java.lang.String")){
			classField.set(passedObj, inputArgs[2]);
			
		} else {
			fieldType = Class.forName("java.lang." + fieldName);
			parseMeth = fieldType.getDeclaredMethod("parse" + fieldName,
					String.class);

			classField.set(passedObj, parseMeth.invoke(fieldType, inputArgs[2]));
		}
	}
}
