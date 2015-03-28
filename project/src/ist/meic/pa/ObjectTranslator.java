package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtField;
import javassist.CtMethod;
import javassist.CtNewMethod;
import javassist.NotFoundException;
import javassist.Translator;

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
		
		//prevent instrumentation of our classes
		String className = ctClass.getName();
		if(className.matches("(.*)ist.meic.pa(.*)") || className.matches("(.*)javassist(.*)")){
			System.out.println("ignored classes: " + className);
			return;
		}

		for (CtMethod ctMethod : ctClass.getDeclaredMethods()) {
			
			String methodName = ctMethod.getName();
			
			//START add new method with $method_name and same body
			System.out.println("copying and changing name of: "+ methodName);
			CtMethod newMethod = CtNewMethod.copy(ctMethod, ctClass, null);
			newMethod.setName("$"+methodName);
			newMethod.insertBefore("{System.out.println(\"ESTOU NA MAIN COPIADA BITCHES!! YEAH!!!\");}");
			ctClass.addMethod(newMethod);
			//END add new method with $method_name and same body
			
			//START change old method body to call Shell
			System.out.println("changing body of: " + ctMethod.getLongName());
			ctMethod.setBody("{{ return ist.meic.pa.Shell.runShell(\""+className+"\",\""+methodName+"\",$args, $sig);}}");
			//END change old method body to call Shell
			
			
		}
	}

}


//old code that can be usefull for sintax
//final String template = "{ist.meic.pa.Shell.runShell(); throw $e; }";
//CtClass etype = ClassPool.getDefault().get("java.lang.Exception");
		
