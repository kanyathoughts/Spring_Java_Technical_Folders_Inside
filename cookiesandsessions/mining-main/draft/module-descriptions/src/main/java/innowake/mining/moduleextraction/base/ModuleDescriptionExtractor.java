package innowake.mining.moduleextraction.base;

import innowake.mining.moduleextraction.util.DescriptionFormatter;

public abstract class ModuleDescriptionExtractor<T> {
	
	protected final RuleSet<T> ruleSet;
	
	public ModuleDescriptionExtractor(RuleSet<T> ruleSet) {
		this.ruleSet = ruleSet;
	}
	
	public String getDescription(String source) {
		// convert the input to the input/output type of the rule-set (T)
		T rulesInput = convertFromString(source);
		
		// apply the rule-set
		T rulesOutput = ruleSet.apply(rulesInput);
		
		// convert the result back to a String
		String output = convertToString(rulesOutput);
		
		// sanitize the result
		String sanitized = sanitize(output);
		
		// format the result
		String formatted = DescriptionFormatter.format(sanitized);
		
		return formatted;
	}

	public abstract String convertToString(T description);

	public abstract T convertFromString(String source);
	
	public abstract String sanitize(String description);
}
