package fw2.orm.xlsx.io;

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import fw2.orm.xlsx.io.mapper.Mapper;

public abstract class WorkbookWriter<T> {

	protected WorkbookLayout layout = null;

	protected XSSFWorkbook workBook = null;
	String fileName = null;
	protected Mapper mapper;

	public WorkbookWriter(final String fileName, final Mapper mapper) {

		this.fileName = fileName;
		this.workBook = new XSSFWorkbook();
		this.layout = WorkbookLayout.getDefaultLayout();
		this.layout.setStyles(this.createStyles());
		this.mapper = mapper;
	}

	public WorkbookWriter(final String fileName, final WorkbookLayout layout, final Mapper mapper) {

		this.fileName = fileName;
		this.layout = layout;
		this.layout.setStyles(this.createStyles());
		this.mapper = mapper;
	}

	public void close() throws IOException {
		final FileOutputStream out = new FileOutputStream(this.fileName);
		this.workBook.write(out);
		out.close();
	}

	public Mapper getMapper() {
		return this.mapper;
	}

	public WorkbookLayout getLayout() {
		return layout;
	}

	public void setLayout(WorkbookLayout layout) {
		this.layout = layout;
	}

	public abstract void writeObject(T object) throws Exception;

	public abstract void writeObjects(List<T> objects) throws Exception;

	private Map<String, CellStyle> createStyles() {
		final Map<String, CellStyle> styles = new HashMap<String, CellStyle>();
		CellStyle style = null;
		Font font = null;
		style = this.workBook.createCellStyle();
		font = this.workBook.createFont();
		font.setColor(IndexedColors.BLACK.getIndex());
		font.setBold(true);
		style.setAlignment(HorizontalAlignment.LEFT);
		style.setFillForegroundColor(IndexedColors.GREY_25_PERCENT.getIndex());
		style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
		style.setBorderBottom(BorderStyle.THIN);
		style.setBorderLeft(BorderStyle.THIN);
		style.setBorderRight(BorderStyle.THIN);
		style.setBorderTop(BorderStyle.THIN);
		style.setFont(font);
		styles.put("Header", style);
		style = this.workBook.createCellStyle();
		font = this.workBook.createFont();
		font.setColor(IndexedColors.BLACK.getIndex());
		style.setAlignment(HorizontalAlignment.LEFT);
		style.setFillForegroundColor(IndexedColors.WHITE.getIndex());
		style.setBorderBottom(BorderStyle.THIN);
		style.setBorderLeft(BorderStyle.THIN);
		style.setBorderRight(BorderStyle.THIN);
		style.setBorderTop(BorderStyle.THIN);
		style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
		style.setFont(font);
		styles.put("Value", style);
		style = this.workBook.createCellStyle();
		font = this.workBook.createFont();
		font.setColor(IndexedColors.DARK_GREEN.getIndex());
		style.setAlignment(HorizontalAlignment.LEFT);
		style.setFillForegroundColor(IndexedColors.WHITE.getIndex());
		style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
		style.setFont(font);
		styles.put("Link", style);
		return styles;
	}
}
