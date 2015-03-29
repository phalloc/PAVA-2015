package ist.meic.pa;

public abstract class Command {

	protected Class<?> cls;
	protected Object passedObj;
	protected String className;
	protected String methodName;
	protected Object[] args;
	protected Class<?>[] argsType;
	protected String[] inputArgs;
	protected Exception e;
	protected static Object returnValue = null;
	
	public void setFields(Class<?> cls, Object passedObj, String className,
			String methodName, Object[] args, Class<?>[] argsType, String[] inputArgs, Exception e){
		this.cls = cls;
		this.passedObj = passedObj;
		this.className = className;
		this.methodName = methodName;
		this.args = args;
		this.argsType = argsType;	
		this.inputArgs = inputArgs;
		this.e = e;
	}
	
	public static Object getReturn(){
		return returnValue;
	}
	
	public static void cleanReturn(){
		returnValue = null;
	}
	public abstract void execute() throws Exception, Throwable;
}
