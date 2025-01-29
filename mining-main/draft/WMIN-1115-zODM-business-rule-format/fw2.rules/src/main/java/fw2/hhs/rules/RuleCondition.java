package fw2.hhs.rules;

public class RuleCondition {
	private String conditionNumber;
	private String term;
	private String referenceTerm;
	private String conditionalOperator;
	private String value;
	private String concatinationOperator;

	public RuleCondition() {
		super();
	}

	public String getConditionNumber() {
		return conditionNumber;
	}

	public void setConditionNumber(String conditionNumber) {
		this.conditionNumber = conditionNumber;
	}

	public String getConditionalOperator() {
		return conditionalOperator;
	}

	public void setConditionalOperator(String conditionalOperator) {
		this.conditionalOperator = conditionalOperator;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public String getConcatinationOperator() {
		return concatinationOperator;
	}

	public void setConcatinationOperator(String concatinationOperator) {
		this.concatinationOperator = concatinationOperator;
	}

	public String getTerm() {
		return term;
	}

	public void setTerm(String term) {
		this.term = term;
	}

	public String getReferenceTerm() {
		return referenceTerm;
	}

	public void setReferenceTerm(String referenceTerm) {
		this.referenceTerm = referenceTerm;
	}

	@Override
	public String toString() {
		return "\n RuleCondition [conditionNumber=" + conditionNumber + ", term=" + term + ", referenceTerm=" + referenceTerm + ", conditionalOperator=" + conditionalOperator
				+ ", value=" + value + ", concatinationOperator=" + concatinationOperator + "]";
	}

}
