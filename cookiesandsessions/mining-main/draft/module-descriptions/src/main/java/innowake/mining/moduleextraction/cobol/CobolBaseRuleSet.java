package innowake.mining.moduleextraction.cobol;

import innowake.mining.moduleextraction.base.RuleSet;

public class CobolBaseRuleSet {
	
	@SuppressWarnings("unchecked")
	public static final RuleSet<String> INSTANCE = RuleSet.of(
				RuleBetweenIDAndEnvironmentDivision::apply,
				RuleComments::apply
			);

	
	private CobolBaseRuleSet() {
		throw new IllegalStateException("Don't construct this class.");
	}
}
