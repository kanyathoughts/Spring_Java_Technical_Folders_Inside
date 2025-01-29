package fw2.orm.xlsx.io;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.poi.ss.usermodel.Sheet;

import fw2.orm.xlsx.io.mapper.Mapper;

public class ObjectReader extends WorkbookReader<Object> {
	private class ObjectSheetReader extends SheetReader {
		InstanceFactory factory;
		public ObjectSheetReader(final Sheet sheet, final Mapper mapper) {
			super(sheet, mapper);
		}

		@Override
		public <T> InstanceFactory getInstanceFactory(final Class<T> type) {
			if (factory ==null) {
				factory = new Factory(mapper);
			}
			return factory;
		}
	}

	public ObjectReader(final InputStream workbookInputStream, final Mapper mapper) throws IOException {
		super(workbookInputStream, mapper);
	}

	public ObjectReader(final InputStream workbookInputStream, final WorkbookLayout layout, final Mapper mapper)
			throws IOException {
		super(workbookInputStream, layout, mapper);
	}

	@Override
	public List<Object> readObjects() throws Exception {

		return new ArrayList<Object>();
	}

	@Override
	public List<Object> readObjects(final List<NameValuePair> keys) throws Exception {

		return new ArrayList<Object>();
	}

	public <T> List<T> readObjects(final String sheetName, final Class<T> type) throws Exception {
		final Sheet sheet = this.workBook.getSheet(sheetName);
		final ObjectSheetReader reader = new ObjectSheetReader(sheet, this.mapper);
		return reader.readObjects(type);
	}

	public <T> List<T> readObjects(final String sheetName, final Class<T> type, final List<NameValuePair> keys)
			throws Exception {
		final Sheet sheet = this.workBook.getSheet(sheetName);
		final ObjectSheetReader reader = new ObjectSheetReader(sheet, this.mapper);
		return reader.readObjects(type, keys);
	}

	@Override
	public Object lookupObject(String sheetName, List<NameValuePair> lookupKeys) {
		// TODO Auto-generated method stub
		return null;
	}
}
