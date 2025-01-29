package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.List;

import org.apache.poi.ss.usermodel.Sheet;

import fw2.orm.xlsx.io.mapper.Mapper;


public abstract class SheetReader {
	protected SheetLayout layout;
	protected RowReader rowReader;
	protected Sheet sheet;
	private final Mapper mapper;

	public SheetReader(final Sheet sheet, final Mapper mapper) {
		this.mapper = mapper;
		this.sheet = sheet;
		this.layout = SheetLayout.getDefaultLayout();
		this.rowReader = new RowReader(sheet);

	}

	public SheetReader(final Sheet sheet, final SheetLayout layout, final Mapper mapper) {
		this.mapper = mapper;
		this.sheet = sheet;
		this.layout = layout;
		this.rowReader = new RowReader(sheet);
	}

	public <T> T createInstance(final Class<T> type, final List<NameValuePair> row) throws Exception {
		final InstanceFactory factory = this.getInstanceFactory(type);
		return factory.createInstance(type, row);
	}

	public <T> List<T> createInstances(final Class<T> type, final List<ArrayList<NameValuePair>> rows)
			throws Exception {
		final InstanceFactory factory = this.getInstanceFactory(type);
		return factory.createInstances(type, rows);
	}

	public abstract <T> InstanceFactory getInstanceFactory(Class<T> type);

	public Mapper getMapper() {
		return this.mapper;
	}

	public RowReader getRowReader() {
		return this.rowReader;
	}

	public <T> List<T> readObjects(final Class<T> type) throws Exception {
		final ArrayList<ArrayList<NameValuePair>> rows = this.layout.readLogicalRows(this.rowReader);
		return this.createInstances(type, rows);
	}

	public <T> List<T> readObjects(final Class<T> type, final List<NameValuePair> keys) throws Exception {
		final ArrayList<ArrayList<NameValuePair>> rows = this.layout.readLogicalRows(this.rowReader, keys);
		final List<T> instances = this.createInstances(type, rows);
		return instances;
	}

	public <T> List<T> readObjects(final Class<T> type, final Object parentObject) throws Exception {
		final List<NameValuePair> keys = this.mapper.flattenReferenceKeys(type, parentObject);
		return this.readObjects(type, keys);
	}
}
