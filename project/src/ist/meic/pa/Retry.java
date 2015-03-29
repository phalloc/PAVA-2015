package ist.meic.pa;

public class Retry extends Command {

	@Override
	public void execute() throws Throwable {
		Shell.stackTrace.remove(Shell.stackTrace.size()-1);
		returnValue = Shell.runShell(passedObj, className, methodName, args,
				argsType);
	}
}
