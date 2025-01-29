package fw2.model2.xlsx.io;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;

import org.apache.poi.ss.usermodel.Sheet;

import fw2.model2.Action;
import fw2.model2.Application;
import fw2.model2.Bap;
import fw2.model2.ComponentMapping;
import fw2.model2.Editor;
import fw2.model2.Field;
import fw2.model2.FieldGroup;
import fw2.model2.SummaryTable;
import fw2.model2.TableColumn;
import fw2.orm.xlsx.io.Factory;
import fw2.orm.xlsx.io.InstanceFactory;
import fw2.orm.xlsx.io.NameValuePair;
import fw2.orm.xlsx.io.SheetReader;
import fw2.orm.xlsx.io.WorkbookReader;
import fw2.orm.xlsx.io.mapper.Mapper;

public class ApplicationXlsxXReader extends WorkbookReader<Application> {
	private class ApplicationSheetReader extends SheetReader {
		public ApplicationSheetReader(final Sheet sheet, final Mapper mapper) {
			super(sheet, mapper);
		}

		@Override
		public <T> InstanceFactory getInstanceFactory(final Class<T> type) {

			return new Factory(ApplicationXlsxXReader.this.mapper);

		}
	}

	public ApplicationXlsxXReader(final InputStream mapFileStream, final Mapper mapper) throws IOException {
		super(mapFileStream, mapper);
	}

	@Override
	public List<Application> readObjects() throws Exception {
		final Sheet sheet = this.workBook.getSheet("Application");

		final ApplicationSheetReader reader = new ApplicationSheetReader(sheet, this.mapper);
		final List<Application> applications = reader.readObjects(Application.class);

		for (final Application application : applications) {
			this.populateApplication(application);
		}
		return applications;
	}

	@Override
	public List<Application> readObjects(final List<NameValuePair> keys) throws Exception {
		return Collections.emptyList();
	}

	public <T> List<T> readObjects(final String sheetName, final Class<T> type, final List<NameValuePair> keys)
			throws Exception {
		final Sheet sheet = this.workBook.getSheet(sheetName);
		final ApplicationSheetReader reader = new ApplicationSheetReader(sheet, this.mapper);
		return reader.readObjects(type, keys);
	}

	public <T> List<T> readObjects(final String sheetName, final Class<T> type, final Object parentObject)
			throws Exception {
		final List<NameValuePair> keys = this.mapper.flattenReferenceKeys(type, parentObject);
		final Sheet sheet = this.workBook.getSheet(sheetName);
		final ApplicationSheetReader reader = new ApplicationSheetReader(sheet, this.mapper);
		return reader.readObjects(type, keys);
	}

	private void populateApplication(final Application application) throws Exception {
		final List<Bap> baps = this.readObjects("Bap", Bap.class, application);

		for (final Bap bap : baps) {
			this.populateBAP(bap);
		}
		application.getBaps().addAll(baps);
	}

	private void populateBAP(final Bap bap) throws Exception {
		this.populateEditor(bap);
		this.populateComponentMapping(bap);
	}

	private void populateComponentMapping(final Bap bap) throws Exception {
		final List<ComponentMapping> componentMappings =
		this.readObjects("ComponentMapping", ComponentMapping.class, bap);
		bap.getComponentMapping().addAll(componentMappings);
	}

	private void populateEditor(final Editor editor) throws Exception {

		final List<SummaryTable> summaryTables = this.readObjects("SummaryTable", SummaryTable.class, editor);
		final List<FieldGroup> fieldGroups = this.readObjects("FieldGroup", FieldGroup.class, editor);
		final List<Editor> editors = this.readObjects("Editor", Editor.class, editor);

		if (summaryTables.size() > 0) {
			this.populateSummaryTables(summaryTables);
			editor.getSummaryTables().addAll(summaryTables);
		}
		for (final FieldGroup fieldGroup : fieldGroups) {
			this.populateFieldGroup(fieldGroup);
		}
		for (final Editor embeddedEditor : editors) {
			this.populateEditor(embeddedEditor);
		}
		editor.getFieldGroups().addAll(fieldGroups);
		editor.getEditors().addAll(editors);
	}

	private void populateFieldGroup(final FieldGroup fieldGroup) throws Exception {
		final List<Field> fields = this.readObjects("Field", Field.class, fieldGroup);
		fieldGroup.getFields().addAll(fields);
		final List<Action> actions = this.readObjects("Action", Action.class, fieldGroup);
		fieldGroup.getActions().addAll(actions);
	}

	private void populateSummaryTables(final List<SummaryTable> summaryTables) throws Exception {
		for (final SummaryTable summaryTable : summaryTables) {
			final List<TableColumn> tableColumns = this.readObjects("TableColumn", TableColumn.class, summaryTable);
			summaryTable.getColumns().addAll(tableColumns);
		}
	}

	@Override
	public Object lookupObject(String sheetName, List<NameValuePair> lookupKeys) {
		// TODO Auto-generated method stub
		return null;
	}
}
