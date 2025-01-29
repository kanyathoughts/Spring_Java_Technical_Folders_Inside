package innowake.mining.moduleextraction.util;

public class DescriptionFormatter {
	
	public static String format(String string) {
		return capitalizeEnglish(string);
	}

	private static String capitalizeEnglish(String string) {
		boolean firstChar = false;
		boolean lastWasDelimiter = false;
		char[] chars = string.toLowerCase().toCharArray();
		for (int i = 0; i < chars.length; i++) {
			if (Character.isAlphabetic(chars[i])) {
				if (!firstChar || lastWasDelimiter) {
					chars[i] = Character.toUpperCase(chars[i]);
					if (!firstChar)
						firstChar = true;
				}
				lastWasDelimiter = false;
			} else if (charIsDelimiter(chars[i])) {
				lastWasDelimiter = true;
			} else if (lastWasDelimiter && !Character.isWhitespace(chars[i])) {
				lastWasDelimiter = false;
			}
		}
		return String.copyValueOf(chars);
	}

	private static boolean charIsDelimiter(char c) {
		return c == '.' || c == '!' || c == '?' || c == ':';
	}
}
