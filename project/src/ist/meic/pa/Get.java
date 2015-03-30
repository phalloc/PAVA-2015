package ist.meic.pa;

import java.lang.reflect.Field;

public class Get extends Command {

	@Override
	public void execute() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		Field f = cls.getDeclaredField(inputArgs[1]);
		f.setAccessible(true);
		System.out.println(f.get(passedObj));
	}
	
}
