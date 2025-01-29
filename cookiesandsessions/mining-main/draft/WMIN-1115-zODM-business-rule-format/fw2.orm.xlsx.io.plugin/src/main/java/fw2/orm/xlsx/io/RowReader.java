package fw2.orm.xlsx.io;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;

public class RowReader {
	Row row;
	int rowIndex = 0;
	Sheet sheet;

	public RowReader(final Sheet sheet) {
		super();
		this.sheet = sheet;
		this.setRowIndex(0);
	}

	public RowReader(final Sheet sheet, final int rowIndex) {
		super();
		this.sheet = sheet;
		this.setRowIndex(rowIndex);
	}

	public int getLastCellNum() {
		return this.getRow().getLastCellNum();
	}

	public Row getNextRow() {
		this.setRowIndex(++this.rowIndex);
		return this.getRow();
	}

	public Row getRow() {
		return this.row;
	}

	public Row getRow(final int rowIndex) {
		this.setRowIndex(rowIndex);
		return this.getRow();
	}

	public int getRowIndex() {
		return this.rowIndex;
	}

	public Sheet getSheet() {
		return this.sheet;
	}

	public String readCellValue(final int cellIndex) {
		final Cell cell = this.getRow().getCell(cellIndex);
		String propertyValue = null;
		if (cell != null) {
			final CellType cellType = cell.getCellType();
			switch (cellType) {
			case STRING:
				propertyValue = cell.getStringCellValue();
				break;
			case NUMERIC:
				final int value = (int) cell.getNumericCellValue();
				propertyValue = String.valueOf(value);
				break;
			case BLANK:
				break;
			case BOOLEAN:
				break;
			case ERROR:
				break;
			case FORMULA:
				break;
			case _NONE:
				break;
			default:
				break;
			}
		}
		return propertyValue;
	}

	public void setRowIndex(final int rowIndex) {
		this.rowIndex = rowIndex;
		this.row = this.sheet.getRow(rowIndex);
	}
}
