package ist.meic.pa;

public class Info implements Command{

	private StackTraceElement[] ste;
	
	@Override
	public void execute() {
		System.err.println("Called object:...");
	}
		

}
