package innowake.mining.moduleextraction.natural;

import innowake.mining.moduleextraction.base.ModuleDescriptionExtractor;
import innowake.mining.moduleextraction.base.RuleSet;

public class NaturallDescriptionExtractor extends ModuleDescriptionExtractor<String> {
	
	public NaturallDescriptionExtractor(RuleSet<String> instance) {
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
		};
		return result.toString();
	}
	
	private static String removeCommentsAndLineNumbersFromString(String s) {
		String result = s;
//		if (result.length() > 72) {
//			result = result.substring(0, 72);
//		}
//		if (result.length() > 7) {
//			result = result.substring(7);
//		}
//		result = result.replace("*", " ").replaceAll("\\s+$", "");
		return result;
	}

}
