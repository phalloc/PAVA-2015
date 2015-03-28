package ist.meic.pa;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.reflect.Method;

import javassist.CtClass;
import javassist.CtMethod;
import javassist.Modifier;

public class Shell {

	public static final Object runShell(Object passedObj, String className, String methodName, Object[] args, Class<?>[] argsType) {		
		
		Object obj = null;
		Class<?> cls = null;
		Method method = null;
		
		try{
			System.out.println("INCLASS: " + className+methodName);
			cls = Class.forName(className);
			if(passedObj == null){		
				obj = cls.newInstance();
			}else obj = passedObj;
			
			method = cls.getDeclaredMethod("$" + methodName, argsType);
			System.out.println("invoking: "+ className + "$" + methodName);
			method.setAccessible(true);
			return method.invoke(obj,args);
			
		} catch(Exception e){
			while (true) {
				System.err.println(e.getCause());
				System.err.print("DebuggerCLI:> ");
				try {
					BufferedReader reader = new BufferedReader(
							new InputStreamReader(System.in));
	
					String input = reader.readLine();
					String[] inputArgs = input.split(" ");
	
					if (inputArgs[0].equals("Abort"))
						return null;
					else if(inputArgs[0].equals("Info")){
						System.out.println("Called object: " + obj);
					}
					
				} catch (Exception ex) {
					System.err.println("This happened in shell.");
				}
			}
		}
	}
}
