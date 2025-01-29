package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.CellStyle;

public class TableLayout {
	LayoutContainer container;
	String name;
	Orientation orientation;
	Location origin;
	SizeType sizeType;
	Map<String, CellStyle> styles = new HashMap<String, CellStyle>();

	public TableLayout(final String name) {
		super();
		this.name = name;
	}

	public int getAbsoluteOriginCellIndex() {
		int cellIndex = this.origin.getCellIndex();
		if (this.container != null) {
			cellIndex += this.container.getAbsoluteOriginCellIndex();
		}
		return cellIndex;
	}

	public int getAbsoluteOriginRowIndex() {
		int rowIndex = this.origin.getRowIndex();
		if (this.container != null) {
			rowIndex += this.container.getAbsoluteOriginRowIndex();
		}
		return rowIndex;
	}

	public LayoutContainer getContainer() {
		return this.container;
	}

	public String getName() {
		return this.name;
	}

	public Orientation getOrientation() {
		return this.orientation;
	}

	public Location getOrigin() {
		return this.origin;
	}

	public SizeType getSizeType() {
		return this.sizeType;
	}

	public CellStyle getStyle(final String style) {

		final CellStyle cellStyle = this.styles.get(style);
		if (cellStyle == null) {
			if (this.getContainer() != null) {
				return this.getContainer().getStyle(style);
			}
		}
		return cellStyle;
	}

	public ArrayList<NameValuePair> readLogicalRow(final RowReader reader, final ArrayList<String> propertyNames,
			final int logicalRowIndex) {
		return this.orientation.readLogicalRow(reader, propertyNames, this, logicalRowIndex);
	}

	public ArrayList<ArrayList<NameValuePair>> readLogicalRows(final RowReader reader) {
		final ArrayList<String> propertyNames = this.orientation.readPropertyNames(reader, this);
		if (propertyNames.size() > 0) {
			return this.sizeType.readLogicalRows(reader, propertyNames, this);
		}
		return new ArrayList<ArrayList<NameValuePair>>();
	}

	public ArrayList<ArrayList<NameValuePair>> readLogicalRows(final RowReader reader, final List<NameValuePair> keys)
			throws Exception {
		ArrayList<ArrayList<NameValuePair>> rows = new ArrayList<ArrayList<NameValuePair>>();
		final ArrayList<String> propertyNames = this.orientation.readPropertyNames(reader, this);
		rows = this.orientation.findMatchingRows(reader, propertyNames, keys, this);
		return rows;
	}

	public ArrayList<NameValuePair> readObject(final RowReader reader, final Location location) {
		ArrayList<NameValuePair> row = new ArrayList<NameValuePair>();
		final ArrayList<String> propertyNames = this.orientation.readPropertyNames(reader, this);
		if (this.orientation instanceof Horizontal) {
			row = this.orientation.readLogicalRow(reader, propertyNames, this, location.getRowIndex());
		} else if (this.orientation instanceof Vertical) {
			row = this.orientation.readLogicalRow(reader, propertyNames, this, location.getCellIndex());
		}
		return row;
	}

	public void setContainer(final LayoutContainer container) {
		this.container = container;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public void setOrientation(final Orientation orientation) {
		this.orientation = orientation;
	}

	public void setOrigin(final Location origin) {
		this.origin = origin;
	}

	public void setSizeType(final SizeType sizeType) {
		this.sizeType = sizeType;
	}

	public void setStyles(final Map<String, CellStyle> styles) {
		this.styles = styles;

	}
	


	public void writeObject(final SheetWriter sheet, final List<NameValuePair> flattenedObject, final boolean writeHeader,
			final int logicalRowIndex) {
		this.orientation.writeObject(sheet, flattenedObject, writeHeader, this, logicalRowIndex);
	}
}
