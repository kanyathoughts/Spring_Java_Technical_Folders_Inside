package WMIN5805;

import org.apache.commons.lang3.StringUtils;

public class Example {
	
	public static void main(String[] args) {
		StringUtils.capitalize("hello world!");
		new Example2().anyMethod();
		Example3.anyStaticMethod();
		new Example3().anyStaticMethod();
	}
}
