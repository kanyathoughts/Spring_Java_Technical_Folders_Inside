package fw2.orm.xlsx.io;

import java.util.List;

import org.apache.poi.ss.usermodel.Sheet;

import fw2.orm.xlsx.io.mapper.Mapper;

public abstract class SheetWriter {
	protected SheetLayout layout;
	boolean headerWritten = false;
	int logicalRowIndex = 0;
	Sheet sheet;
	private final Mapper mapper;
	List<NameValuePair> header;

	public SheetWriter(final Sheet sheet, final Mapper mapper) {
		this.mapper = mapper;
		this.sheet = sheet;
		this.layout = SheetLayout.getDefaultLayout();


	}

	public SheetWriter(final Sheet sheet, final SheetLayout layout, final Mapper mapper) {
		this.mapper = mapper;
		this.sheet = sheet;
		this.layout = layout;
	}

	public Mapper getMapper() {
		return this.mapper;
	}

	public void writeObject(final Object object) throws Exception {
		final List<NameValuePair> flattenedObject = this.mapper.flatten(object);
		this.writeRow(flattenedObject);
	}

	public void writeObjects(final List<Object> objects) throws Exception {
		for (final Object object : objects) {
			final List<NameValuePair> flattenedObject = this.mapper.flatten(object);
			this.writeRow(flattenedObject);
		}
	}
	

	public void writeRow(final List<NameValuePair> row) throws Exception {
		if (!this.headerWritten) {
			this.layout.writeObject(this, row, true, this.logicalRowIndex++);
			this.headerWritten = true;
		} else {
			this.layout.writeObject(this, row, false, this.logicalRowIndex++);
		}
	}

	public void writeRows(final List<List<NameValuePair>> rows) throws Exception {
		for (final List<NameValuePair> row : rows) {
			this.writeRow(row);
		}
	}

	public List<NameValuePair> getHeader() {
		return header;
	}

	public void setHeader(List<NameValuePair> header) {
		this.header = header;
	}
}
