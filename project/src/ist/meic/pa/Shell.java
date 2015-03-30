package ist.meic.pa;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import javassist.Modifier;

public class Shell {

	public static List<String> stackTrace = new ArrayList<String>();

	public static void stackInit(String className, String methodName, Object[] args) {

		String res = className + "." + methodName + "(";
		for (Object o : args) {
			res += o + ",";
		}
		res += ")";
		res = res.replace(",)", ")");

		stackTrace.add(res);

	}

	public static final Object runShell(Object passedObj, String className,
			String methodName, Object[] args, Class<?>[] argsType)
			throws Throwable {

		Object obj = null;
		Class<?> cls = null;
		Method method = null;

		addStack(className, methodName, args);

		try {
			cls = Class.forName(className);
			method = cls.getDeclaredMethod("" + methodName, argsType);
			method.setAccessible(true);

			Object res = null;

			//parse method and object to understand if the class is or not static
			//in order to invoke with or without an instance
			if (passedObj == null && Modifier.isStatic(method.getModifiers())) {
				obj = cls;
				res = method.invoke(cls, args);
			} else if (passedObj == null) {
				throw new NullPointerException();
			} else {
				obj = passedObj;
				res = method.invoke(obj, args);
			}

			stackTrace.remove(stackTrace.size() - 1);
			return res;

		} catch (NullPointerException npe) {
			stackTrace.remove(stackTrace.size() - 1);
			throw npe.getCause();
		} catch (Exception e) {
			System.err.println(e.getCause());

			while (true) {
				System.err.print("DebuggerCLI:> ");

				BufferedReader reader = new BufferedReader(
						new InputStreamReader(System.in));

				String input = reader.readLine();
				String[] inputArgs = input.split(" ");

				
				//command invocation
				
				//fixed invocation in the case of throw
				if (inputArgs[0].equals("Throw")) {
					stackTrace.remove(stackTrace.size() - 1);
					throw e.getCause();
					
				//dynamic invocation with the use of the command pattern
				} else {
					Class<?> command = Class.forName("ist.meic.pa."
							+ inputArgs[0]);
					Object commandObj = command.newInstance();
					Method meth = null;

					meth = command.getMethod("setFields", Class.class,
							Object.class, String.class, String.class,
							Object[].class, Class[].class, String[].class,
							Exception.class);
					meth.invoke(commandObj, cls, passedObj, className,
							methodName, args, argsType, inputArgs, e);

					meth = command.getMethod("execute");

					meth.invoke(commandObj);
					meth = command.getMethod("getReturn");
					Object ret = meth.invoke(null);

					if (ret != null) {
						meth = command.getMethod("cleanReturn");
						meth.invoke(null);
						return ret;
					}
				}
			}
		}
	}

	//adds the method calls to the callStack
	public static void addStack(String className, String methodName,
			Object[] args) {
		String res = className + "." + methodName + "(";
		for (Object o : args) {
			res += o + ", ";
		}
		res += ")";
		res = res.replace(", )", ")");

		stackTrace.add(res);
	}
}
