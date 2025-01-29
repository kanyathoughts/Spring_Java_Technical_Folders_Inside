package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.List;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;



public class Horizontal extends Orientation {
	public Horizontal() {
		super();
		this.typeName = "Horizontal";
	}

	@Override
	public ArrayList<ArrayList<NameValuePair>> findMatchingRows(final RowReader reader,
			final ArrayList<String> propertyNames, final List<NameValuePair> keys, final TableLayout tableLayout)
					throws Exception {
		final ArrayList<ArrayList<NameValuePair>> rows = new ArrayList<ArrayList<NameValuePair>>();
		final int firstRowIndex = tableLayout.getAbsoluteOriginRowIndex();
		final int firstCellIndex = tableLayout.getAbsoluteOriginCellIndex();
		final Row readerRow = reader.getRow(firstRowIndex);
		if (readerRow != null) {
			final int lastCellIndex = reader.getLastCellNum();
			final int numLogicalRows = lastCellIndex - firstCellIndex - 1;
			for (int i = 0; i < numLogicalRows; i++) {
				final ArrayList<NameValuePair> row = this.readLogicalRow(reader, propertyNames, tableLayout, i);
				if (this.rowMatchesKeys(keys, row)) {
					rows.add(row);
				}
			}
		}
		return rows;
	}

	@Override
	public ArrayList<NameValuePair> readLogicalRow(final RowReader reader, final ArrayList<String> propertyNames,
			final TableLayout tableLayout, final int logicalRowIndex) {
		final ArrayList<NameValuePair> logicalRow = new ArrayList<NameValuePair>();
		final int firstRowIndex = tableLayout.getAbsoluteOriginRowIndex();
		final int firstCellIndex = tableLayout.getAbsoluteOriginCellIndex();
		for (int i = 0; i < propertyNames.size(); i++) {
			reader.setRowIndex(firstRowIndex + i);
			final String propertyValue = reader.readCellValue(firstCellIndex + 1 + logicalRowIndex);
			final String columnName = propertyNames.get(i);
			final NameValuePair entry = new NameValuePair(columnName, propertyValue);
			logicalRow.add(entry);
		}
		return logicalRow;
	}

	@Override
	public ArrayList<String> readPropertyNames(final RowReader reader, final TableLayout tableLayout) {
		final int firstRowIndex = tableLayout.getAbsoluteOriginRowIndex();
		final int firstCellIndex = tableLayout.getAbsoluteOriginCellIndex();
		int index = firstRowIndex;
		final ArrayList<String> columnNames = new ArrayList<String>();
		while (reader.getRow(index) != null) {
			final String cellValue = reader.readCellValue(firstCellIndex);
			if ((cellValue == null) || ((cellValue != null) && (cellValue.trim().length() == 0))) {
				break;
			}
			if (cellValue != null) {
				columnNames.add(cellValue);
			}
			index++;
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
		int rowIndex = firstRowIndex;
		final int cellIndex = firstCellIndex + logicalRowIndex;
		for (final NameValuePair nameValuePair : flattenedObject) {
			Row valueRow = sheet.getRow(rowIndex);
			if (valueRow == null) {
				valueRow = sheet.createRow(rowIndex);
			}
			rowIndex++;
			final Cell valueCell = valueRow.createCell(cellIndex);
			// TODO fix valueCell.setCellStyle(tableLayout.getStyle("Header"));
			valueCell.setCellValue(nameValuePair.getName());
		}
	}

	private void writeValues(final Sheet sheet, final List<NameValuePair> flattenedObject,
			final TableLayout tableLayout, final int logicalRowIndex) {
		final int firstRowIndex = tableLayout.getAbsoluteOriginRowIndex();
		final int firstCellIndex = tableLayout.getAbsoluteOriginCellIndex();
		int rowIndex = firstRowIndex;
		final int cellIndex = firstCellIndex + logicalRowIndex;
		for (final NameValuePair nameValuePair : flattenedObject) {
			Row valueRow = sheet.getRow(rowIndex);
			if (valueRow == null) {
				valueRow = sheet.createRow(rowIndex);
			}
			rowIndex++;
			final Cell valueCell = valueRow.createCell(cellIndex);
			// TODO fix valueCell.setCellStyle(tableLayout.getStyle("Value"));
			valueCell.setCellValue(nameValuePair.getValue());
		}
	}
}
