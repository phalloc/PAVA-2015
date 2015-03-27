package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
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
		
		//prevent instumentation of the Shell class
		if(ctClass.getName().equals("ist.meic.pa.Shell"))
			return;
		
		//old code that can be usefull for sintax
		//final String template = "{ist.meic.pa.Shell.runShell(); throw $e; }";
		//CtClass etype = ClassPool.getDefault().get("java.lang.Exception");
		
		for (CtMethod ctMethod : ctClass.getDeclaredMethods()) {
			System.out.println(ctMethod.getName());
			
		}
	}

}
