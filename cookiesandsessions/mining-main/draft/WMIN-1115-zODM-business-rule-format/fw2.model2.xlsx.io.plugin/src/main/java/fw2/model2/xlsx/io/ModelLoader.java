package fw2.model2.xlsx.io;

import fw2.model2.Model;
import fw2.model2.Model2Package;
import fw2.xmi.XMILoader;

public class ModelLoader extends XMILoader<Model> {

	public ModelLoader() {
		super();
		Model2Package.eINSTANCE.eClass();
	}

}



