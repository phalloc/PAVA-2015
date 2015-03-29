package ist.meic.pa;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import javassist.Modifier;

public class Shell {

	private static List<String> stackTrace = new ArrayList<String>();

	public static void stackInit(String className, String methodName,
			Object[] args) {

		String res = className + "." + methodName + "(";
		for(Object o : args){
			res += o + ", ";
		}
		res += ")";
		res = res.replace(", )", ")");
		
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

			if (passedObj == null && Modifier.isStatic(method.getModifiers())) {
				obj = cls;
				res = method.invoke(cls, args);
			} else if (passedObj == null) {
				throw new NullPointerException();
			} else {
				obj = passedObj;
				res = method.invoke(obj, args);
			}

			stackTrace.remove(stackTrace.size()-1);
			return res;

		} catch (NullPointerException npe) {
			stackTrace.remove(stackTrace.size()-1);
			throw npe.getCause();
		} catch (Exception e) {
			System.err.println(e.getCause());

			while (true) {
				System.err.print("DebuggerCLI:> ");

				BufferedReader reader = new BufferedReader(
						new InputStreamReader(System.in));

				String input = reader.readLine();
				String[] inputArgs = input.split(" ");

				// ////////////// COMMANDS ////////////////
				if (inputArgs[0].equals("Abort")) {
					System.exit(-1);
				} else if (inputArgs[0].equals("Info")) {
					String result = "Called object: " + passedObj + "\n";

					result += "\tFields: ";

					for (Field f : cls.getDeclaredFields()) {
						result += f.getName() + " ";
					}

					result += "\nCalled Stack: \n";

					for (int i = stackTrace.size()-1; i >= 0; i--) {
						result += stackTrace.get(i) + "\n";
					}

					System.out.println(result);
				} else if (inputArgs[0].equals("Get")) {
					Field f = cls.getDeclaredField(inputArgs[1]);
					f.setAccessible(true);
					System.out.println(f.get(obj));
				} else if (inputArgs[0].equals("Set")) {

					Field f = cls.getDeclaredField(inputArgs[1]);
					Class<?> fieldType = f.getType();
					f.setAccessible(true);
					String fieldName = fieldType.getName();
					String op = "";
					String cl = "";

					if (fieldName.equals("int")) {
						cl = "Integer";
						op = "Int";
					} else if (fieldName.equals("double")) {
						op = "Double";
						cl = op;
					}

					Class<?> cfield = Class.forName("java.lang." + cl);

					Method fieldmeth = cfield.getDeclaredMethod("parse" + op,
							String.class);
					f.set(obj, fieldmeth.invoke(cfield, inputArgs[2]));
				} else if (inputArgs[0].equals("Return")) {
					stackTrace.remove(stackTrace.size()-1);
					Class<?> returnType = method.getReturnType();

					String fieldName = returnType.getName();
					String op = "";
					String cl = "";

					if (fieldName.equals("int")) {
						cl = "Integer";
						op = "Int";
					} else if (fieldName.equals("double")) {
						op = "Double";
						cl = op;
					} else if (fieldName.equals("String")) {
						return inputArgs[1];
					}

					Class<?> cfield = Class.forName("java.lang." + cl);
					Method fieldmeth = cfield.getDeclaredMethod("parse" + op,
							String.class);

					return fieldmeth.invoke(cfield, inputArgs[1]);
				} else if (inputArgs[0].equals("Retry")) {
					stackTrace.remove(stackTrace.size()-1);
					return runShell(passedObj, className, methodName, args,
							argsType);
				} else if (inputArgs[0].equals("Throw")) {
					stackTrace.remove(stackTrace.size()-1);
					throw e.getCause();

				}

			}

		}

	}

	private static void addStack(String className, String methodName,
			Object[] args) {
		String res = className + "." + methodName + "(";
		for(Object o : args){
			res += o + ", ";
		}
		res += ")";
		res = res.replace(", )", ")");
		
		stackTrace.add(res);
	}
}
