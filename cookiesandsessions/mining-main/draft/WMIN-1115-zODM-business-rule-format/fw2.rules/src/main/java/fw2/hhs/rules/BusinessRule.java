package fw2.hhs.rules;

import java.util.ArrayList;
import java.util.List;

public class BusinessRule {
	
	private String rulePackage;
	private String ruleName;
	
	private String ruleDocumentation;
	
	private String priority;
	
	private List<BusinessRuleStatement> businessRuleStatements = new ArrayList<BusinessRuleStatement>();
//	private RuleDefinition ruleDefinitions=new RuleDefinition();;// = new LinkedHashSet<RuleDefinition>() ;
//	private RuleCondition ruleConditions=new RuleCondition();// = new ArrayList<RuleCondition>();
//	private RuleAction ruleActions=new RuleAction();// = new ArrayList<RuleAction>();
	private List<RuleDefinition> ruleDefinitions = new ArrayList<RuleDefinition>() ;
	private List<RuleCondition> ruleConditions = new ArrayList<RuleCondition>();
	private List<RuleAction> ruleActions = new ArrayList<RuleAction>();

	public String getRulePackage() {
		return rulePackage;
	}

	public void setRulePackage(String rulePackage) {
		this.rulePackage = rulePackage;
	}

	public String getRuleName() {
		return ruleName;
	}

	public void setRuleName(String ruleName) {
		this.ruleName = ruleName;
	}

	public void setRuleDocumentation(String ruleDocumentation) {
		this.ruleDocumentation = ruleDocumentation;
	}

	public void setPriority(String priority) {
		this.priority = priority;
	}

	public String getPriority() {
		return priority;
	}

	public String getRuleDocumentation() {
		return ruleDocumentation;
	}

	public List<BusinessRuleStatement> getBusinessRuleStatements() {
		return businessRuleStatements;
	}

	public void setBusinessRuleStatements(
			List<BusinessRuleStatement> businessRuleStatements) {
		this.businessRuleStatements = businessRuleStatements;
	}

	public List<RuleDefinition> getRuleDefinitions() {
		return ruleDefinitions;
	}

	public void setRuleDefinitions(List<RuleDefinition> ruleDefinitions) {
		this.ruleDefinitions = ruleDefinitions;
	}

	public List<RuleCondition> getRuleConditions() {
		return ruleConditions;
	}

	public void setRuleConditions(List<RuleCondition> ruleConditions) {
		this.ruleConditions = ruleConditions;
	}

	public List<RuleAction> getRuleActions() {
		return ruleActions;
	}

	public void setRuleActions(List<RuleAction> ruleActions) {
		this.ruleActions = ruleActions;
	}

	
	@Override
	public boolean equals(Object object) {

		
		if (object instanceof BusinessRule) {
			BusinessRule businessRule = (BusinessRule) object;
			if (this.rulePackage != null
					&& this.ruleName != null) {
				if (this.rulePackage.equals(businessRule
								.getRulePackage())
						&& this.ruleName.equals(businessRule.getRuleName())) {
					return true;
				}
			}
		}
		return false;

	}

	@Override
	public int hashCode() {
		int hash =7;
		
	
		hash = 31* hash+ (this.rulePackage==null ? 0:  this.rulePackage.hashCode() );
		hash = 31* hash+ (this.ruleName==null ? 0:  this.ruleName.hashCode() );
		

		return hash;
	}

//	public void setRuleDefinitions(RuleDefinition ruleDefinitions) {
//		this.ruleDefinitions = ruleDefinitions;
//	}
//
//	public RuleDefinition getRuleDefinitions() {
//		return ruleDefinitions;
//	}
//
//	public void setRuleConditions(RuleCondition ruleConditions) {
//		this.ruleConditions = ruleConditions;
//	}
//
//	public RuleCondition getRuleConditions() {
//		return ruleConditions;
//	}
//
//	public void setRuleActions(RuleAction ruleActions) {
//		this.ruleActions = ruleActions;
//	}
//
//	public RuleAction getRuleActions() {
//		return ruleActions;
//	}

//	@Override
//	public int compareTo(BusinessRule object) {
//
//		if (object instanceof BusinessRule) {
//			BusinessRule businessRule = (BusinessRule) object;
//			if (this.ruleProject != null && this.rulePackage != null
//					&& this.ruleName != null) {
//				if (this.ruleProject.equals(businessRule.getRuleProject())
//						&& this.rulePackage.equals(businessRule
//								.getRulePackage())
//						&& this.ruleName.equals(businessRule.getRuleName())) {
//					return 1;
//				}
//			}
//		}
//		return -1;
//	}
}
