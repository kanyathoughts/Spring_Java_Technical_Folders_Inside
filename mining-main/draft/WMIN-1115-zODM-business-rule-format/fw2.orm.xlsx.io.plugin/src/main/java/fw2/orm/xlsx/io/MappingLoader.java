package fw2.orm.xlsx.io;

import fw2.orm.xlsx.Mapping;
import fw2.orm.xlsx.XlsxPackage;
import fw2.xmi.XMILoader;

public class MappingLoader extends XMILoader<Mapping> {

	public MappingLoader() {
		super();
		XlsxPackage.eINSTANCE.eClass();
	}

}
