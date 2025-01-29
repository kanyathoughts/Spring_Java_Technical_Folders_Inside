package innowake.mining.moduleextraction.cobol;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RuleBetweenIDAndEnvironmentDivision{

	private static final Pattern regex = Pattern.compile(".*ID(ENTIFICATION)?\\s+DIVISION\\.(?<content>.*)ENVIRONMENT\\s+DIVISION.*", Pattern.DOTALL);
	
	public static String apply(String t) {
		String filtered = betweenIDandEnv(t);
		return filtered;
	}

	private static String betweenIDandEnv(String s) {
		Matcher m = regex.matcher(s);
		return m.matches() ? m.group("content") : null;
	}
}
