package fw2.hhs.rules;

public class BusinessRuleStatement {

	private Integer statementId;
	private String definitionTerm;
	private String definitionType;
	private String collectionReferenceTerm;
	private String definitionReferenceElementTerm;
	private String definitionReferenceHolderTerm;
	private String conditionTerm;
	private String conditionReferenceTerm;
	private String conditionalOperator;
	private String conditionValue;
	private String concatinationOperator;
	private String actionTerm;
	private String actionReferenceTerm;

	private String actionValue;

	public BusinessRuleStatement() {
		super();
		this.statementId = 0;
		this.definitionTerm = "";
		this.definitionType = "";
		this.collectionReferenceTerm = "";
		this.definitionReferenceElementTerm = "";
		this.definitionReferenceHolderTerm = "";
		this.conditionTerm = "";
		this.conditionReferenceTerm = "";
		this.conditionalOperator = "";
		this.conditionValue = "";
		this.concatinationOperator = "";
		this.actionTerm = "";
		this.actionReferenceTerm = "";
		this.actionValue = "";
	}

	public Integer getStatementId() {
		return statementId;
	}

	public void setStatementId(Integer statementId) {
		this.statementId = statementId;
	}

	public String getConcatinationOperator() {
		return concatinationOperator;
	}

	public String getConditionalOperator() {
		return conditionalOperator;
	}

	public String getConditionReferenceTerm() {
		return conditionReferenceTerm;
	}

	public String getConditionTerm() {
		return conditionTerm;
	}

	public String getDefinitionTerm() {
		return definitionTerm;
	}

	public String getDefinitionType() {
		return definitionType;
	}

	public void setConcatinationOperator(String concatinationOperator) {
		this.concatinationOperator = concatinationOperator;
	}

	public void setConditionalOperator(String conditionalOperator) {
		this.conditionalOperator = conditionalOperator;
	}

	public void setConditionReferenceTerm(String conditionReferenceTerm) {
		this.conditionReferenceTerm = conditionReferenceTerm;
	}

	public void setConditionTerm(String conditionTerm) {
		this.conditionTerm = conditionTerm;
	}

	public String getDefinitionReferenceElementTerm() {
		return definitionReferenceElementTerm;
	}

	public void setDefinitionReferenceElementTerm(
			String definitionReferenceElementTerm) {
		this.definitionReferenceElementTerm = definitionReferenceElementTerm;
	}

	public String getDefinitionReferenceHolderTerm() {
		return definitionReferenceHolderTerm;
	}

	public void setDefinitionReferenceHolderTerm(
			String definitionReferenceHolderTerm) {
		this.definitionReferenceHolderTerm = definitionReferenceHolderTerm;
	}

	public void setDefinitionTerm(String definitionTerm) {
		this.definitionTerm = definitionTerm;
	}

	public void setDefinitionType(String definitionType) {
		this.definitionType = definitionType;
	}

	public String getConditionValue() {
		return conditionValue;
	}

	public void setConditionValue(String conditionValue) {
		this.conditionValue = conditionValue;
	}

	public String getActionTerm() {
		return actionTerm;
	}

	public void setActionTerm(String actionTerm) {
		this.actionTerm = actionTerm;
	}

	public String getActionReferenceTerm() {
		return actionReferenceTerm;
	}

	public void setActionReferenceTerm(String actionReferenceTerm) {
		this.actionReferenceTerm = actionReferenceTerm;
	}

	public String getActionValue() {
		return actionValue;
	}

	public void setActionValue(String actionValue) {
		this.actionValue = actionValue;
	}

	public void setCollectionReferenceTerm(String collectionReferenceTerm) {
		this.collectionReferenceTerm = collectionReferenceTerm;
	}

	public String getCollectionReferenceTerm() {
		return collectionReferenceTerm;
	}

}
