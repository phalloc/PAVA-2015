package ist.meic.pa;

public class Abort extends Command {

	@Override
	public void execute() {
		System.exit(-1);
	}

}
