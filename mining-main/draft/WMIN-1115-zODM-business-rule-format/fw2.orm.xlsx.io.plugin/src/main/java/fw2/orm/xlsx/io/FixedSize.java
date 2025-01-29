package fw2.orm.xlsx.io;

import java.util.ArrayList;

public class FixedSize extends SizeType {
	public FixedSize() {
		super();
		this.typeName = "FixedSize";
		this.size = 1;
	}

	@Override
	public ArrayList<ArrayList<NameValuePair>> readLogicalRows(final RowReader reader,
			final ArrayList<String> propertyNames, final TableLayout tableLayout) {
		final ArrayList<ArrayList<NameValuePair>> result = new ArrayList<ArrayList<NameValuePair>>();
		for (int i = 0; i < this.size; i++) {
			result.add(tableLayout.readLogicalRow(reader, propertyNames, i));
		}
		return result;
	}
}
