package innowake.mining.moduleextraction.natural;

public class RuleComments {
	
	public static String apply(String t) {
		StringBuilder ret = new StringBuilder(t.length());
		String[] lines = t.split(System.lineSeparator());
		for (String line : lines) {
			if (line.startsWith("*") || line.startsWith("/*") ) {
				ret.append(line);
				ret.append(System.lineSeparator());
			}
		}
		return ret.toString();
	}
}
