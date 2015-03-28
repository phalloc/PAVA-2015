package ist.meic.pa;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.reflect.Method;

import javassist.CtClass;

public class Shell {

	public static final void runShell(String className, String methodName, Object[] args, Class<?>[] argsType) {
		try{
			
			Class<?> cls = Class.forName(className);
			Object obj = cls.newInstance(); 
			Method method =	cls.getDeclaredMethod("$"+methodName, argsType);
			System.out.println("invoking: "+ className + "$" + methodName);
			method.invoke(obj,args);
			
		} catch(Exception e){
			while (true) {
				e.printStackTrace();
				System.err.print("DebuggerCLI:> ");
				try {
					BufferedReader reader = new BufferedReader(
							new InputStreamReader(System.in));
	
					String input = reader.readLine();
					String[] inputArgs = input.split(" ");
	
					if (inputArgs[0].equals("Abort"))
						return;
	
					Class<?> objClass = Class
							.forName("ist.meic.pa." + inputArgs[0]);
	
					Command obj = (Command) objClass.newInstance();
					Method meth = objClass.getMethod("execute");
					meth.invoke(obj);
					
				} catch (Exception ex) {
					System.err.println("This happened in shell.");
				}
			}
		}
	}
}
