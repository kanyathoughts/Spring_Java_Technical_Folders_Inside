package fw2.orm.xlsx.io;

public class Location {
	int cellIndex;
	int rowIndex;

	public Location(final int rowIndex, final int cellIndex) {
		this.cellIndex = cellIndex;
		this.rowIndex = rowIndex;
	}

	public int getCellIndex() {
		return this.cellIndex;
	}

	public int getRowIndex() {
		return this.rowIndex;
	}

	public void setCellIndex(final int cellIndex) {
		this.cellIndex = cellIndex;
	}

	public void setRowIndex(final int rowIndex) {
		this.rowIndex = rowIndex;
	}
}
