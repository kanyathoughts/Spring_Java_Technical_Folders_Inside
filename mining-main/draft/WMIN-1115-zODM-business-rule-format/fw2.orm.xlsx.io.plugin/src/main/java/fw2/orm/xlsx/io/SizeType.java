package fw2.orm.xlsx.io;

import java.util.ArrayList;

public abstract class SizeType {
	int size;
	String typeName;

	public SizeType() {
		super();
	}

	public String getTypeName() {
		return this.typeName;
	}

	public abstract ArrayList<ArrayList<NameValuePair>> readLogicalRows(RowReader reader,
			ArrayList<String> propertyNames, TableLayout tableLayout);

	public void setTypeName(final String typeName) {
		this.typeName = typeName;
	}
}
