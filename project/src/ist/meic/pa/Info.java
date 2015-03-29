package ist.meic.pa;

import java.lang.reflect.Field;

public class Info extends Command{

	
	@Override
	public void execute() {
		String result = "Called object: " + passedObj + "\n";

		result += "\tFields: ";

		for (Field f : cls.getDeclaredFields()) {
			result += f.getName() + " ";
		}

		result += "\nCalled Stack: \n";

		for (int i = Shell.stackTrace.size()-1; i >= 0; i--) {
			result += Shell.stackTrace.get(i) + "\n";
		}
		System.out.println(result);
	}

}
