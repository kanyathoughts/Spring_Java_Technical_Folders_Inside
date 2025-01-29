package innowake.mining.moduleextraction.natural;

import innowake.mining.moduleextraction.base.RuleSet;

public class NaturalBaseRuleSet {
	
	@SuppressWarnings("unchecked")
	public static final RuleSet<String> INSTANCE = RuleSet.of(
				RuleBeforeDefinineData::apply,
				RuleComments::apply
			);

	
	private NaturalBaseRuleSet() {
		throw new IllegalStateException("Don't construct this class.");
	}
}
