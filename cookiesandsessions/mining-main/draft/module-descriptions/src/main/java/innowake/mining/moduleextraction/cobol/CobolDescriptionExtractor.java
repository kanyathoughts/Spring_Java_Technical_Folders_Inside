package innowake.mining.moduleextraction.cobol;

import innowake.mining.moduleextraction.base.ModuleDescriptionExtractor;
import innowake.mining.moduleextraction.base.RuleSet;

public class CobolDescriptionExtractor extends ModuleDescriptionExtractor<String> {
	
	public CobolDescriptionExtractor(RuleSet<String> instance) {
		super(instance);
	}

	@Override
	public String convertToString(String description) {
		return description;
		
	}

	@Override
	public String convertFromString(String source) {
		return source;
	}

	@Override
	public String sanitize(String description) {
		String[] lines = description.split("\\r?\\n");
		StringBuilder result = new StringBuilder(description.length()/2);
		for (String s : lines) {
			result.append(removeCommentsAndLineNumbersFromString(s));
			result.append(System.lineSeparator());
		};
		return result.toString();
		
		
//		return Arrays.stream(lines).map(x -> removeCommentsAndLineNumbersFromString(x))
//				.reduce((s1, s2) -> s1 + System.lineSeparator() + s2)
//				.get();
	}
	
	private static String removeCommentsAndLineNumbersFromString(String s) {
		String result = s;
		if (result.length() > 72) {
			result = result.substring(0, 72);
		}
		if (result.length() > 7) {
			result = result.substring(7);
		}
		result = result.replace("*", " ").replaceAll("\\s+$", "");
		return result;
	}

}
