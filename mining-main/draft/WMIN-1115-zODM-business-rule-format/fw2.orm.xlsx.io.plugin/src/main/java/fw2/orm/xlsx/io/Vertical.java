package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.List;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;

public class Vertical extends Orientation {
	public Vertical() {
		super();
		this.typeName = "Vertical";
	}

	@Override
	public ArrayList<ArrayList<NameValuePair>> findMatchingRows(final RowReader reader,
			final ArrayList<String> propertyNames, final List<NameValuePair> keys, final TableLayout tableLayout)
					throws Exception {
		final ArrayList<ArrayList<NameValuePair>> rows = new ArrayList<ArrayList<NameValuePair>>();
		int index = 0;
		ArrayList<NameValuePair> logicalRow = tableLayout.readLogicalRow(reader, propertyNames, index);
		while ((logicalRow != null) && !NameValuePair.isEmpty(logicalRow)) {
			if (this.rowMatchesKeys(keys, logicalRow)) {
				rows.add(logicalRow);
			}
			logicalRow = tableLayout.readLogicalRow(reader, propertyNames, ++index);
		}
		return rows;
	}

	@Override
	public ArrayList<NameValuePair> readLogicalRow(final RowReader reader, final ArrayList<String> propertyNames,
			final TableLayout tableLayout, final int logicalRowIndex) {
		final ArrayList<NameValuePair> row = new ArrayList<NameValuePair>();
		final int absoluteOriginRowIndex = tableLayout.getAbsoluteOriginRowIndex();
		final Row valueRow = reader.getRow(absoluteOriginRowIndex + 1 + logicalRowIndex);
		if (valueRow != null) {
			final int num = reader.getLastCellNum();
			final int absoluteOriginCellIndex = tableLayout.getAbsoluteOriginCellIndex();
			for (int i = absoluteOriginCellIndex; (i < num)
					&& (i < (absoluteOriginCellIndex + propertyNames.size())); i++) {
				final String propertyValue = reader.readCellValue(i);
				final String columnName = propertyNames.get(i - absoluteOriginCellIndex);
				final NameValuePair entry = new NameValuePair(columnName, propertyValue);
				row.add(entry);
			}
		}
		return row;
	}

	@Override
	public ArrayList<String> readPropertyNames(final RowReader reader, final TableLayout tableLayout) {
		final int firstRowIndex = tableLayout.getAbsoluteOriginRowIndex();
		final int firstCellIndex = tableLayout.getAbsoluteOriginCellIndex();
		final Row headerRow = reader.getRow(firstRowIndex);
		final ArrayList<String> columnNames = new ArrayList<String>();
		if (headerRow != null) {
			final int num = reader.getLastCellNum();
			for (int i = firstCellIndex; i < num; i++) {
				final String cellValue = reader.readCellValue(i);

				if ((cellValue == null) || ((cellValue != null) && (cellValue.trim().length() == 0))) {
					break;
				}

				if (cellValue != null) {
					columnNames.add(cellValue);
				}
			}
		}
		return columnNames;
	}

	@Override
	public void writeObject(final SheetWriter sheet, final List<NameValuePair> flattenedObject, final boolean writeHeader,
			final TableLayout tableLayout, final int logicalRowIndex) {
		if (writeHeader) {
			this.writeHeader(sheet.sheet, sheet.header, tableLayout, logicalRowIndex);
		}
		this.writeValues(sheet.sheet, flattenedObject, tableLayout, logicalRowIndex + 1);
	}
	

	public void writeHeader(final Sheet sheet, final List<NameValuePair> flattenedObject,
			final TableLayout tableLayout, final int logicalRowIndex) {
		final int firstRowIndex = tableLayout.getAbsoluteOriginRowIndex();
		final int firstCellIndex = tableLayout.getAbsoluteOriginCellIndex();
		final Row valueRow = sheet.createRow(logicalRowIndex + firstRowIndex);
		int cellIndex = firstCellIndex;
		for (final NameValuePair nameValuePair : flattenedObject) {
			final Cell valueCell = valueRow.createCell(cellIndex++);
			valueCell.setCellStyle(tableLayout.getStyle("Header"));
			valueCell.setCellValue(nameValuePair.getName());
		}
	}

	private void writeValues(final Sheet sheet, final List<NameValuePair> flattenedObject,
			final TableLayout tableLayout, final int logicalRowIndex) {
		final int firstRowIndex = tableLayout.getAbsoluteOriginRowIndex();
		final int firstCellIndex = tableLayout.getAbsoluteOriginCellIndex();
		final Row valueRow = sheet.createRow(logicalRowIndex + firstRowIndex);
		int cellIndex = firstCellIndex;
		for (final NameValuePair nameValuePair : flattenedObject) {
			final Cell valueCell = valueRow.createCell(cellIndex++);
			valueCell.setCellStyle(tableLayout.getStyle("Value"));
			valueCell.setCellValue(nameValuePair.getValue());
		}
	}
}
