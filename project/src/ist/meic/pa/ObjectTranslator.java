package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.Translator;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;

public class ObjectTranslator implements Translator {

	@Override
	public void onLoad(ClassPool pool, String className)
			throws NotFoundException, CannotCompileException {
		CtClass ctClass = pool.get(className);
		instrument(ctClass);
	}

	@Override
	public void start(ClassPool arg0) throws NotFoundException,
			CannotCompileException {
	}

	void instrument(CtClass ctClass) throws NotFoundException,
			CannotCompileException {

		// prevent instrumentation of the debugger classes
		final String className = ctClass.getName();
		if (className.matches("(.*)ist.meic.pa(.*)")
			|| className.matches("(.*)javassist(.*)")
			|| className.matches("(.*)java(.*)")) {
			return;
		}

		for (CtMethod ctMethod : ctClass.getDeclaredMethods()) {

			String methodName = ctMethod.getName();

			//since main is not invoked, this method adds the function and its arguments to the stack
			if(methodName.equals("main")){
				ctMethod.insertBefore("ist.meic.pa.Shell.stackInit(\""+className+"\",\""+methodName+"\", $1);");
			}

			//replace method calls with calls to our debugger (providing the method name and its arguments)
			ctMethod.instrument(new ExprEditor() {
				public void edit(MethodCall m) throws CannotCompileException {
					if (!m.getClassName().matches("(.*)ist.meic.pa(.*)")) {
						try {
							m.replace("{{ $_ = ($r) ($w) ist.meic.pa.Shell.runShell($0,\""
									+ m.getMethod().getDeclaringClass()
											.getName()
									+ "\",\""
									+ m.getMethodName() + "\",$args, $sig); }}");
						} catch (NotFoundException e) {
							e.printStackTrace();
						}
					}
				}
			});

		}

	}
}
