public class JavaEntry1 {

	static int a = 1;

	public int sum() {
		int b = 2;
		return a + b;
	}

	public String display() {
		String str1 = "Hello";
		String str2 = "World";
		return str1 + str2;
	}

	public static void main(final String args[]) {
		JavaEntry1 o = new JavaEntry1();
		System.out.println(o.display());
		System.out.println(o.sum());
	}
}
