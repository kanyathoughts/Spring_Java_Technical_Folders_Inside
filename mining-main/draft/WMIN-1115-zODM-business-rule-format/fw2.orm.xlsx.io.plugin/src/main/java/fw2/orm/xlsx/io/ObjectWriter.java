package fw2.orm.xlsx.io;

import java.util.List;

import org.apache.poi.ss.usermodel.PrintSetup;
import org.apache.poi.ss.usermodel.Sheet;

import fw2.orm.xlsx.io.mapper.Mapper;

public class ObjectWriter extends WorkbookWriter<Object> {
	private class ObjectSheetWriter extends SheetWriter {
		public ObjectSheetWriter(final Sheet sheet, final Mapper mapper) {
			super(sheet, mapper);
			ObjectWriter.this.layout.addSheetLayout(this.layout); // Hack for
			// style
		}

		public ObjectSheetWriter(final Sheet sheet, final SheetLayout layout, final Mapper mapper) {
			super(sheet, layout, mapper);
		}
	}

	public ObjectWriter(final String fileName, final Mapper mapper) {
		super(fileName, mapper);
	}

	public ObjectWriter(final String fileName, final WorkbookLayout layout, final Mapper mapper) {
		super(fileName, layout, mapper);
	}

	public ObjectSheetWriter createSheetWriter(final String sheetName) {
		Sheet sheet;
		sheet = this.workBook.createSheet(sheetName);
		final PrintSetup printSetup = sheet.getPrintSetup();
		printSetup.setLandscape(true);
		sheet.setFitToPage(true);
		sheet.setHorizontallyCenter(true);
		final ObjectSheetWriter sheetWriter = new ObjectSheetWriter(sheet, this.mapper);
		return sheetWriter;
	}

	@Override
	public void writeObject(final Object object) throws Exception {
		throw new UnsupportedOperationException();
	}

	@Override
	public void writeObjects(final List objects) throws Exception {
		final Object car = CollectionUtil.car(objects);
		final ObjectSheetWriter sheetWriter = this.createSheetWriter(car.getClass().getSimpleName());
		sheetWriter.writeObjects(objects);
	}
	
	public void writeObjects(final List objects, String sheetName) throws Exception {
		final Object car = CollectionUtil.car(objects);
		final ObjectSheetWriter sheetWriter = this.createSheetWriter(sheetName);
		sheetWriter.writeObjects(objects);
	}
}
