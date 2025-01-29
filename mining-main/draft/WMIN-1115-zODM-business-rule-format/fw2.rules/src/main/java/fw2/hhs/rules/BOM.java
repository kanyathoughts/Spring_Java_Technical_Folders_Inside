package fw2.hhs.rules;

import java.util.ArrayList;
import java.util.List;

public class BOM {

	String bomName;
	List<BOMClass> bomClasses = new ArrayList<BOMClass>();

	public List<BOMClass> getBomClasses() {
		return bomClasses;
	}

	public void setBomClasses(List<BOMClass> bomClasses) {
		this.bomClasses = bomClasses;
	}

	public String getBomName() {
		return bomName;
	}

	public void setBomName(String bomName) {
		this.bomName = bomName;
	}

}
