package fw2.hhs.rules;

public class RuleAction {
	private String actionNumber;
	private String term;
	private String referenceTerm;
	private String value;

	public RuleAction() {
		super();
		actionNumber = "";
		term = "";
		referenceTerm = "";
		value = "";
	}

	public String getActionNumber() {
		return actionNumber;
	}

	public void setActionNumber(String actionNumber) {
		this.actionNumber = actionNumber;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
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

}
