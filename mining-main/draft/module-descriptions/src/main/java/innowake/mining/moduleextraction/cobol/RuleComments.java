package innowake.mining.moduleextraction.cobol;

public class RuleComments {
	
	public static String apply(String t) {
		StringBuilder ret = new StringBuilder(t.length());
		String[] lines = t.split("\\r?\\n");
		for (String line : lines) {
			if (line.length() >= 6 && line.charAt(6) == '*') {
				ret.append(line);
				ret.append(System.lineSeparator());
			}
		}
		return ret.toString();
	}
}
