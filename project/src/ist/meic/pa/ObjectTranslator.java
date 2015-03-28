package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.CtNewMethod;
import javassist.Modifier;
import javassist.NotFoundException;
import javassist.Translator;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;
import javassist.expr.NewExpr;

public class ObjectTranslator implements Translator {

	@Override
	public void onLoad(ClassPool pool, String className)
			throws NotFoundException, CannotCompileException {
		CtClass ctClass = pool.get(className);
		method(ctClass);
	}

	@Override
	public void start(ClassPool arg0) throws NotFoundException,
			CannotCompileException {
		// TODO Auto-generated method stub

	}

	void method(CtClass ctClass) throws NotFoundException,
			CannotCompileException {

		// prevent instrumentation of our classes
		final String className = ctClass.getName();
		if (className.matches("(.*)ist.meic.pa(.*)")
				|| className.matches("(.*)javassist(.*)")) {
			System.out.println("ignored classes: " + className);
			return;
		}


		for (CtMethod ctMethod : ctClass.getDeclaredMethods()) {
			
			String methodName = ctMethod.getName();
			String temp = null;
			if(Modifier.isStatic(ctMethod.getModifiers())){
				temp = "{{ return ($r) ist.meic.pa.Shell.runShell(null,\""
					+ className + "\",\"" + methodName + "\",$args, $sig);}}";
			}
			else temp = "{{ return ($r) ist.meic.pa.Shell.runShell($0,\""
					+ className + "\",\"" + methodName + "\",$args, $sig);}}";
			final String template = temp;

			// START add new method with $method_name and same body
			System.out.println("copying and changing name of: " + methodName);
			CtMethod newMethod = CtNewMethod.copy(ctMethod, ctClass, null);
			newMethod.setName("$" + methodName);
//			newMethod.setModifiers(Modifier.PUBLIC);
			newMethod.insertBefore("{System.out.println(\"ESTOU NA MAIN COPIADA BITCHES!! YEAH!!!\");}");
			System.out.println("modifirers: "+newMethod.getModifiers());
			ctClass.addMethod(newMethod);
			// END add new method with $method_name and same body

			// START change old method body to call Shell
			System.out.println("changing body of: " + ctMethod.getLongName());

			ctMethod.setBody(template);
			// END change old method body to call Shell
			
//			newMethod.instrument(
//				    new ExprEditor() {
//				    	public void edit(NewExpr n) {
//				    		
//				    	}
//				        public void edit(MethodCall m)
//				                      throws CannotCompileException
//				        {
//				            if (m.getClassName().equals("Point")
//				                          && m.getMethodName().equals("move"))
//				                m.replace("{ $1 = 0; $_ = $proceed($$); }");
//				        }
//				    });

		}
		

	}

}

// old code that can be usefull for sintax
// final String template = "{ist.meic.pa.Shell.runShell(); throw $e; }";
// CtClass etype = ClassPool.getDefault().get("java.lang.Exception");

