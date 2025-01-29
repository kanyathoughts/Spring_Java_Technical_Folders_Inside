package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.List;

import org.apache.poi.ss.usermodel.Sheet;

public abstract class Orientation {
	String typeName;

	public abstract ArrayList<ArrayList<NameValuePair>> findMatchingRows(RowReader reader,
			ArrayList<String> propertyNames, List<NameValuePair> keys, TableLayout tableLayout) throws Exception;

	public String getTypeName() {
		return this.typeName;
	}

	public abstract ArrayList<NameValuePair> readLogicalRow(RowReader reader, ArrayList<String> propertyNames,
			TableLayout tableLayout, int logicalRowIndex);

	public abstract ArrayList<String> readPropertyNames(RowReader reader, TableLayout tableLayout);

	public boolean rowMatchesKeys(final List<NameValuePair> keys, final ArrayList<NameValuePair> logicalRow) {
		if (logicalRow.containsAll(keys)) {
			return true;
		} else {
			return false;
		}
	}

	public void setTypeName(final String typeName) {
		this.typeName = typeName;
	}
	
	
	public abstract void writeObject(SheetWriter sheet, List<NameValuePair> flattenedObject, boolean writeHeader,
			TableLayout tableLayout, int logicalRowIndex);
}
