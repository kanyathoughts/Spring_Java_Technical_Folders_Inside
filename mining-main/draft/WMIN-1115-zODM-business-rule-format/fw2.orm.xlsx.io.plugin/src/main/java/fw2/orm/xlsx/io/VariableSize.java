package fw2.orm.xlsx.io;

import java.util.ArrayList;

public class VariableSize extends SizeType {
	public VariableSize() {
		super();
		this.typeName = "VariableSize";
	}

	@Override
	public ArrayList<ArrayList<NameValuePair>> readLogicalRows(final RowReader reader,
			final ArrayList<String> propertyNames, final TableLayout tableLayout) {
		final ArrayList<ArrayList<NameValuePair>> result = new ArrayList<ArrayList<NameValuePair>>();
		int index = 0;
		ArrayList<NameValuePair> logicalRow = null;
		logicalRow = tableLayout.readLogicalRow(reader, propertyNames, index++);
		while ((logicalRow != null) && !NameValuePair.isEmpty(logicalRow)) {
			result.add(logicalRow);
			logicalRow = tableLayout.readLogicalRow(reader, propertyNames, index++);
		}
		return result;
	}
}
