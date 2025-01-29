package practice;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Regular_Expressions_Practice {
	
	public static void main(String[] args) {
//		String s1 = "2024-07-01";
//		System.out.println(s1.matches("(\\d{4})[\\-.](\\d{1,2})[\\-.](\\d{1,2})"));
		// Actually we have 4 groups but we have specified ?: in the group to make it non-capturing group
		// so groupCount will be 3 only
		// group(0) will be the entire match
		Pattern pattern = Pattern.compile("(\\d{4})([\\-.])(\\d{1,2})([\\-.])(\\d{1,2})");
		Matcher matcher = pattern.matcher("2024-07-01");
		System.out.println(matcher.matches());
		System.out.println(matcher.groupCount());
		for (int i = 0; i <= matcher.groupCount(); i++) {
			System.out.println("i value is:" + i + " and group is |" + matcher.group(i) + "|");
		}
	}

}
