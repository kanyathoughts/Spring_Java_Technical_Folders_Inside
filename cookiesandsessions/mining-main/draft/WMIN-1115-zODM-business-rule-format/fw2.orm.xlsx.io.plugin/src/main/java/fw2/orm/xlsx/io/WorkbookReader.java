package fw2.orm.xlsx.io;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import fw2.orm.xlsx.io.mapper.Mapper;

public abstract class WorkbookReader<T> {
	protected WorkbookLayout layout = null;
	protected XSSFWorkbook workBook = null;
	protected Mapper mapper;
	InputStream workbookInputStream = null;

	public WorkbookReader() {
		// TODO Auto-generated constructor stub
	}

	public WorkbookReader(final InputStream workbookInputStream, final Mapper mapper) throws IOException {

		this.layout = WorkbookLayout.getDefaultLayout();
		this.workbookInputStream = workbookInputStream;
		this.workBook = new XSSFWorkbook(workbookInputStream);
		this.mapper = mapper;
		this.mapper.initLookupStrategy(this);
	}

	public WorkbookReader(final InputStream workbookInputStream, final WorkbookLayout layout, final Mapper mapper)
			throws IOException {

		this.layout = layout;
		this.workbookInputStream = workbookInputStream;
		this.workBook = new XSSFWorkbook(workbookInputStream);
		this.mapper = mapper;
		this.mapper.initLookupStrategy(this);
	}

	public Mapper getMapper() {
		return this.mapper;
	}

	public abstract List<T> readObjects() throws Exception;

	public abstract List<T> readObjects(List<NameValuePair> keys) throws Exception;

	public abstract Object lookupObject(String sheetName, List<NameValuePair> lookupKeys);
}
