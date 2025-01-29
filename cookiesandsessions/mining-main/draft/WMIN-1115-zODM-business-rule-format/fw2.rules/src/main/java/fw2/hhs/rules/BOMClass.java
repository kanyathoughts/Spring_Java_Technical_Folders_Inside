package fw2.hhs.rules;

import java.util.ArrayList;
import java.util.List;

public class BOMClass {

	private String packageName;
	private String className;
	private List<BOMElement> bomElements = new ArrayList<BOMElement>();;
	private String verbalization;

	public String getPackageName() {
		return packageName;
	}

	public void setPackageName(String packageName) {
		this.packageName = packageName;
	}

	public String getClassName() {
		return className;
	}

	public void setClassName(String className) {
		this.className = className;
	}

	public List<BOMElement> getBomElements() {
		return bomElements;
	}

	public void setBomElements(List<BOMElement> bomElements) {
		this.bomElements = bomElements;
	}

	public String getVerbalization() {
		return verbalization;
	}

	public void setVerbalization(String verbalization) {
		this.verbalization = verbalization;
	}

}
