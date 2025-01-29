package fw2.hhs.rules;

import java.util.ArrayList;
import java.util.List;

import fw2.hhs.decisiontables.DecisionTable;

public class RuleProject {

	String repositoryLocation;
	String ruleProjectName;
	List<BOM> bom = new ArrayList<BOM>();
	List<BusinessRule> rules = new ArrayList<BusinessRule>();
	List<DecisionTable> decisionRules = new ArrayList<DecisionTable>();
	List<Variable> variableSet = new ArrayList<Variable>();
	String direction;

	public String getRepositoryLocation() {
		return repositoryLocation;
	}

	public void setRepositoryLocation(String repositoryLocation) {
		this.repositoryLocation = repositoryLocation;
	}

	public String getRuleProjectName() {
		return ruleProjectName;
	}

	public void setRuleProjectName(String ruleProjectName) {
		this.ruleProjectName = ruleProjectName;
	}

	public List<BOM> getBom() {
		return bom;
	}

	public void setBom(List<BOM> bom) {
		this.bom = bom;
	}

	public List<BusinessRule> getRules() {
		return rules;
	}

	public void setRules(List<BusinessRule> rules) {
		this.rules = rules;
	}

	public List<DecisionTable> getDecisionRules() {
		return decisionRules;
	}

	public void setDecisionRules(List<DecisionTable> decisionRules) {
		this.decisionRules = decisionRules;
	}

	public List<Variable> getVariableSet() {
		return variableSet;
	}

	public void setVariableSet(List<Variable> variableSet) {
		this.variableSet = variableSet;
	}

	public String getDirection() {
		return direction;
	}

	public void setDirection(String direction) {
		this.direction = direction;
	}
}
