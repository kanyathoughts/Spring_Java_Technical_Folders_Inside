package fw2.hhs.rules;

public class RuleDefinition implements Comparable<RuleDefinition> {
	private String definitionNumber;
	private String term;
	private String collectionReferenceTerm;
	private String referenceElementTerm;
	private String referenceHolderTerm;
	private String definitionType;
	private String filter;

	public RuleDefinition() {
		super();
		term = "";
		referenceElementTerm = "";
		referenceHolderTerm = "";
		collectionReferenceTerm="";
		definitionType = "";
		setFilter("");
	}

	public String getDefinitionNumber() {
		return definitionNumber;
	}

	public void setDefinitionNumber(String definitionNumber) {
		this.definitionNumber = definitionNumber;
	}

	public String getTerm() {
		return term;
	}

	public void setTerm(String term) {
		this.term = term;
	}


	public String getReferenceElementTerm() {
		return referenceElementTerm;
	}

	public void setReferenceElementTerm(String referenceElementTerm) {
		this.referenceElementTerm = referenceElementTerm;
	}

	public String getReferenceHolderTerm() {
		return referenceHolderTerm;
	}

	public void setReferenceHolderTerm(String referenceHolderTerm) {
		this.referenceHolderTerm = referenceHolderTerm;
	}

	public String getDefinitionType() {
		return definitionType;
	}

	public void setDefinitionType(String definitionType) {
		this.definitionType = definitionType;
	}

	public void setCollectionReferenceTerm(String collectionReferenceTerm) {
		this.collectionReferenceTerm = collectionReferenceTerm;
	}

	public String getCollectionReferenceTerm() {
		return collectionReferenceTerm;
	}

	public void setFilter(String filter) {
		this.filter = filter;
	}

	public String getFilter() {
		return filter;
	}

	@Override
	public boolean equals(Object object) {
		System.out.println("Equals called");
		
		if (object instanceof RuleDefinition) {
			RuleDefinition ruleDefinition = (RuleDefinition) object;
			if (this.term != null && this.referenceElementTerm != null&& this.referenceHolderTerm != null
					&& this.definitionType != null) {
				if (this.term.equals(ruleDefinition.getTerm())
						&& this.referenceElementTerm.equals(ruleDefinition
								.getReferenceElementTerm())
														&& this.referenceHolderTerm.equals(ruleDefinition
								.getReferenceHolderTerm())
						&& this.definitionType.equals(ruleDefinition
								.getDefinitionType())) {
					return true;
				}
			}
		}
		return false;

	}

	@Override
	public int hashCode() {
		int hash =7;

		hash = 31* hash+ (this.term==null ? 0:  this.term.hashCode() );
		hash = 31* hash+ (this.referenceElementTerm==null ? 0:  this.referenceElementTerm.hashCode() );
		hash = 31* hash+ (this.referenceHolderTerm==null ? 0:  this.referenceHolderTerm.hashCode() );
		hash = 31* hash+ (this.definitionType==null ? 0:  this.definitionType.hashCode() );
/*		if (this.term != null) {
			hashString += this.term;
//			hash = 31* hash+ this.term.hashCode();
		}
//		System.out.println(" hashCode : " + hashString.hashCode());
		if (this.referenceTerm != null) {
			hashString += "|"+this.referenceTerm;
			hash = 63* hash+ this.referenceTerm.hashCode();
		}
//		System.out.println(" hashCode : " + hashString.hashCode());

		if (this.definitionType != null) {
			hashString += "|"+this.definitionType;
			hash = 63* hash+ this.definitionType.hashCode();
		}
//		System.out.println(" hashCode : " + hashString.hashCode());

//		System.out.println(" hashCode : " + hashString.hashCode());
//*/	//	System.out.println(" hashCode : " + hash);
		return hash;//hashString.hashCode();
	}

	@Override
	public int compareTo(RuleDefinition object) {
//		System.out.println("compareTo called");
		// TODO Auto-generated method stub
		if (object instanceof RuleDefinition) {
			RuleDefinition ruleDefinition = (RuleDefinition) object;
			if (this.term != null && this.referenceElementTerm != null&& this.referenceHolderTerm != null
					&& this.definitionType != null) {
				if (this.term.equals(ruleDefinition.getTerm())
						&& this.referenceElementTerm.equals(ruleDefinition
								.getReferenceElementTerm())
														&& this.referenceHolderTerm.equals(ruleDefinition
								.getReferenceHolderTerm())
						&& this.definitionType.equals(ruleDefinition
								.getDefinitionType())) {
					return 0;
				}
			}
		}
		return -1;
	}

}
