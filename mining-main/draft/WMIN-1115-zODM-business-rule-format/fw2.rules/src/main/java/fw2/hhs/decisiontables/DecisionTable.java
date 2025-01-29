package fw2.hhs.decisiontables;

import java.util.ArrayList;
import java.util.List;

import fw2.hhs.rules.RuleCondition;
import fw2.hhs.rules.RuleDefinition;
import fw2.orm.xlsx.io.NameValuePair;

public class DecisionTable {
	private String ruleProjectName;
	private String rulePackage;
	private String decisionTableName;
	private String documentation;
	private List<RuleDefinition> ruleDefinitions = new ArrayList<RuleDefinition>();
	private List<RuleCondition> ruleConditions = new ArrayList<RuleCondition>();
	private List<ConditionColumn> conditionColumns = new ArrayList<ConditionColumn>();
	private List<ActionColumn> actionColumns = new ArrayList<ActionColumn>();
	private ArrayList<ArrayList<NameValuePair>> conditionData = new ArrayList<ArrayList<NameValuePair>>();
	private ArrayList<ArrayList<NameValuePair>> actionData = new ArrayList<ArrayList<NameValuePair>>();

	// private List<List<String>> actionData=new ArrayList<List<String>>();

	public DecisionTable() {
		ruleProjectName = "";
		rulePackage = "";
		decisionTableName = "";
		documentation = "";
	}

	public String getDecisionTableName() {
		return decisionTableName;
	}

	public void setDecisionTableName(String decisionTableName) {
		this.decisionTableName = decisionTableName;
	}

	public String getDocumentation() {
		return documentation;
	}

	public String getRulePackage() {
		return rulePackage;
	}

	public void setRulePackage(String rulePackage) {
		this.rulePackage = rulePackage;
	}

	public void setDocumentation(String documentation) {
		this.documentation = documentation;
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

	public List<ConditionColumn> getConditionColumns() {
		return conditionColumns;
	}

	public void setConditionColumns(List<ConditionColumn> conditionColumns) {
		this.conditionColumns = conditionColumns;
	}

	public List<ActionColumn> getActionColumns() {
		return actionColumns;
	}

	public void setActionColumns(List<ActionColumn> actionColumns) {
		this.actionColumns = actionColumns;
	}

	public ArrayList<ArrayList<NameValuePair>> getConditionData() {
		return conditionData;
	}

	public void setConditionData(
			ArrayList<ArrayList<NameValuePair>> conditionData) {
		this.conditionData = conditionData;
	}

	//
	// public List<List<String>> getActionData() {
	// return actionData;
	// }
	//
	// public void setActionData(List<List<String>> actionData) {
	// this.actionData = actionData;
	// }

	public void setActionData(ArrayList<ArrayList<NameValuePair>> actionData) {
		this.actionData = actionData;
	}

	public ArrayList<ArrayList<NameValuePair>> getActionData() {
		return actionData;
	}

	public void setRuleProjectName(String ruleProjectName) {
		this.ruleProjectName = ruleProjectName;
	}

	public String getRuleProjectName() {
		return ruleProjectName;
	}

}
