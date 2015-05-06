package ist.meic.pa;

import java.lang.reflect.Field;

public class Info extends Command{

	@Override
	public void execute() {
		String result = "Called Object: " + passedObj + "\n";
		result += "       Fields: ";

		for (Field f : cls.getDeclaredFields()) {
			result += f.getName() + " ";
		}

		result += "\nCall Stack: \n";

		for (int i = Shell.stackTrace.size()-1; i >= 0; i--) {
			result += Shell.stackTrace.get(i) + "\n";
		}
		
		System.out.println(result);
	}

}
