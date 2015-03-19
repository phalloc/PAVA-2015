package DebuggerCLI;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class Main{
	
	public static void main(String[] args) throws ClassNotFoundException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		System.out.println("ola");
		Method[] methods = null;
		Class<?> myclass = Class.forName(args[0]);
		methods = myclass.getDeclaredMethods();
		for(Method m : methods){
			if(m.getName() == "main"){
				m.invoke(null);
			}
		}		
	}
	
}