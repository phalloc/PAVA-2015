package ist.meic.pa;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class Return extends Command {

	@Override
	public void execute() throws NoSuchMethodException, SecurityException,
			ClassNotFoundException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {

		Shell.stackTrace.remove(Shell.stackTrace.size() - 1);
		Method calledMeth = cls.getDeclaredMethod("" + methodName, argsType);
		calledMeth.setAccessible(true);
		
		Class<?> returnType = calledMeth.getReturnType();
		Class<?> rtClass;
		Method parseMeth;

		//Uppercase the first character
		String rtName = returnType.getName();
		rtName = rtName.substring(0, 1).toUpperCase() + rtName.substring(1);

		if (rtName.equals("Int") || rtName.equals("Integer")) {
			rtClass = Class.forName("java.lang.Integer");
			parseMeth = rtClass.getDeclaredMethod("parseInt",
					String.class);
			returnValue = parseMeth.invoke(rtClass, inputArgs[1]);
		} else if (rtName.equals("String")) {
			returnValue = inputArgs[1];
		} else {

			rtClass = Class.forName("java.lang." + rtName);
			parseMeth = rtClass.getDeclaredMethod("parse" + rtName,
					String.class);

			returnValue = parseMeth.invoke(rtClass, inputArgs[1]);
		}
	}
}
