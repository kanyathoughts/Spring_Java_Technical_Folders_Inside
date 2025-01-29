package fw2.orm.xlsx.io;

import fw2.orm.xlsx.Mapping;
import fw2.orm.xlsx.XlsxPackage;
import fw2.xmi.XMILoader2;

public class MappingLoader2 extends XMILoader2<Mapping> {

	public MappingLoader2() {
		super();
		XlsxPackage.eINSTANCE.eClass();
	}

}
