public class JavaEntry3 {

	private void check() {
		System.out.println("Check implementation");
	}

	protected void test() {
		check();
		System.out.println("Test the implementation");
	}

	public static void main(final String args[]) {
		test();
	}
}
