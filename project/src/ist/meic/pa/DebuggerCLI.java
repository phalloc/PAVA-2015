package ist.meic.pa;

import javassist.ClassPool;
import javassist.Loader;
import javassist.Translator;

public class DebuggerCLI {

	public static void main(String[] args) throws Throwable {
		String[] restArgs = new String[args.length - 1];
		System.arraycopy(args, 1, restArgs, 0, restArgs.length);

		Translator translator = new ObjectTranslator();
		ClassPool cp = ClassPool.getDefault();
		
		Loader cl = new Loader(cp);
		cl.addTranslator(cp, translator);

		try {
			cl.run(args[0], restArgs);
		} catch (Exception e) {
				System.err.println(e.getCause());
		}
	}
}