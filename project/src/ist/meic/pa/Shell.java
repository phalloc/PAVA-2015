package ist.meic.pa;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.reflect.Method;

import javassist.CtClass;

public class Shell {

	public static final void runShell() {
		
		while (true) {

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
				
			} catch (Exception e) {
				System.err.println("This happened in shell.");
			}
		}
	}
}
