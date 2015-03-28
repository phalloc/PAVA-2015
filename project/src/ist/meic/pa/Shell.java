package ist.meic.pa;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.TreeMap;

public class Shell {
	
	private static Map<Integer, String> mapid = new TreeMap<Integer, String>();
	private static Map<String, Object[]> mapargs = new TreeMap<String, Object[]>();

	public static void stackInit(String className, String methodName, Object[] args){

		mapid.put(1, className+"."+methodName);
		mapargs.put(className+"."+methodName, args);
	}
	
	public static final Object runShell(Object passedObj, String className,
			String methodName, Object[] args, Class<?>[] argsType)
			throws Throwable {
		
				
		mapid.put(mapid.size()+1, className+"."+methodName); 
		mapargs.put(className+"."+methodName, args);
		
		Object obj = null;
		Class<?> cls = null;
		Method method = null;

		try {
			System.out.println("INCLASS: " + className + methodName);
			cls = Class.forName(className);
			if (passedObj == null) {
				obj = cls.newInstance();
			} else
				obj = passedObj;

			method = cls.getDeclaredMethod("" + methodName, argsType);
			System.out.println("invoking: " + className + "$" + methodName);
			method.setAccessible(true);
			return method.invoke(obj, args);

		} catch (Exception e) {
			System.err.println(e.getCause());

			while (true) {
				System.err.print("DebuggerCLI:> ");

				BufferedReader reader = new BufferedReader(
						new InputStreamReader(System.in));

				String input = reader.readLine();
				String[] inputArgs = input.split(" ");

				if (inputArgs[0].equals("Abort")) {
					System.exit(-1);
				} else if (inputArgs[0].equals("Info")) {
					String result = "Called object: " + obj + "\n";

					for (Field f : cls.getDeclaredFields()) {
						result += "\tFields: " + f.getName();
					}

					
					result += "\nCalled Stack: \n";
			
			        for(Integer i=mapid.size(); i>0;i--){
			        	result += mapid.get(i) + "(";
			        	for(Object o : mapargs.get(mapid.get(i))){
							result += o + ", ";
						}
			        	result += ")\n";
			        	result = result.replace(", )", ")");
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
					
					mapargs.remove(mapid.get(mapid.size()));
					mapid.remove(mapid.size());

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
					}

					Class<?> cfield = Class.forName("java.lang." + cl);
					Method fieldmeth = cfield.getDeclaredMethod("parse" + op,
							String.class);

					return fieldmeth.invoke(cfield, inputArgs[1]);
				} else if (inputArgs[0].equals("Retry")) {
					return method.invoke(obj, args);
				} else if (inputArgs[0].equals("Throw")) {
					mapargs.remove(mapid.get(mapid.size()));
					mapid.remove(mapid.size());
					throw e.getCause();

				}

			}

		}
	}
}
