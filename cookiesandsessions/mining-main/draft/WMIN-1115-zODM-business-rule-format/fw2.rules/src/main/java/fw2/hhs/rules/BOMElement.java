package fw2.hhs.rules;

public class BOMElement {

//	private String[] modifers;
	private String dataType;
	private String elementName;
	private String verbalization;
	private String isStatic;
	private String isFinal;
	private String className;

	public BOMElement() {
		isStatic = "N";
		setIsFinal("N");
	}
//	public String[] getModifers() {
//		return modifers;
//	}
//
//	public void setModifers(String[] modifers) {
//		this.modifers = modifers;
//	}

	public String getDataType() {
		return dataType;
	}

	public String getIsStatic() {
		return isStatic;
	}

	public void setIsStatic(String isStatic) {
		this.isStatic = isStatic;
	}

	public void setIsFinal(String isFinal) {
		this.isFinal = isFinal;
	}

	public String getIsFinal() {
		return isFinal;
	}

	public void setDataType(String dataType) {
		this.dataType = dataType;
	}

	public String getElementName() {
		return elementName;
	}

	public void setElementName(String elementName) {
		this.elementName = elementName;
	}

	public String getVerbalization() {
		return verbalization;
	}

	public void setVerbalization(String verbalization) {
		this.verbalization = verbalization;
	}

	public String getClassName() {
		return className;
	}

	public void setClassName(String className) {
		this.className = className;
	}

}
