package fw2.orm.xlsx.io;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import org.apache.poi.ss.usermodel.Sheet;
import org.eclipse.emf.ecore.EObject;

import fw2.orm.xlsx.AggregationMap;
import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.ColumnMap;
import fw2.orm.xlsx.ComponentMap;
import fw2.orm.xlsx.LookupKey;
import fw2.orm.xlsx.LookupMap;
import fw2.orm.xlsx.ReferenceKey;
import fw2.orm.xlsx.ReferenceMap;
import fw2.orm.xlsx.io.mapper.Mapper;

public class ClassMapXlsxReader extends WorkbookReader<ClassMap> {
	private class ClassMapSheetReader extends SheetReader {

		private InstanceFactory componentMapFactory;
		private InstanceFactory factory;

		public ClassMapSheetReader(final Sheet sheet, final Mapper mapper) {
			super(sheet, mapper);
		}

		@Override
		public <T> InstanceFactory getInstanceFactory(final Class<T> type) {

			if (type == ComponentMap.class) {
				if (this.componentMapFactory == null) {
					this.componentMapFactory = new ComponentMapFactory(mapper);
				}
				return this.componentMapFactory;

			} else {
				if (this.factory == null) {
					this.factory = new Factory(mapper);
				}
				return this.factory;
			}
		}
	}

	private static final String COLUMN_MAP = "ColumnMap";

	public ClassMapXlsxReader() {
		super();
	}

	public ClassMapXlsxReader(final InputStream workbookInputStream, final Mapper mapper) throws IOException {
		super(workbookInputStream, mapper);
	}

	@Override
	public List<ClassMap> readObjects() throws Exception {
		final Sheet classMapSheet = this.workBook.getSheet("ClassMap");
		final ClassMapSheetReader reader = new ClassMapSheetReader(classMapSheet, this.mapper);
		final List<ClassMap> classMaps = reader.readObjects(ClassMap.class);
		
		mapper.addLookupObjects("ClassMap", classMaps);

		for (final ClassMap classMap : classMaps) {
			this.populateClassMap(classMap);
		}
		return classMaps;
	}

	@Override
	public List<ClassMap> readObjects(final List<NameValuePair> keys) throws Exception {
		final Sheet sheet = this.workBook.getSheet("ClassMap");
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		final List<ClassMap> classMaps = reader.readObjects(ClassMap.class, keys);
		for (final ClassMap classMap : classMaps) {
			this.populateClassMap(classMap);
		}
		return classMaps;
	}

	public <T> List<T> readObjects(final String sheetName, final Class<T> type) throws Exception {
		final Sheet sheet = this.workBook.getSheet(sheetName);
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		return reader.readObjects(type);
	}

	public <T> List<T> readObjects(final String sheetName, final Class<T> type, final EObject parentObject)
			throws Exception {
		final List<NameValuePair> keys = this.mapper.flattenReferenceKeys(type, parentObject);
		final Sheet sheet = this.workBook.getSheet(sheetName);
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		return reader.readObjects(type, keys);
	}

	public <T> List<T> readObjects(final String sheetName, final Class<T> type, final List<NameValuePair> keys)
			throws Exception {

		final Sheet sheet = this.workBook.getSheet(sheetName);
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		return reader.readObjects(type, keys);
	}

	public <T> List<T> readObjects(final String sheetName, final Class<T> type, final Object parentObject)
			throws Exception {
		final List<NameValuePair> keys = this.mapper.flattenReferenceKeys(type, parentObject);
		final Sheet sheet = this.workBook.getSheet(sheetName);
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		return reader.readObjects(type, keys);
	}

	private void populateClassMap(final ClassMap classMap) throws Exception {
		final List<ColumnMap> columnMaps = this.readObjects(ClassMapXlsxReader.COLUMN_MAP, ColumnMap.class, classMap);
		final List<AggregationMap> aggregationMaps = this.readObjects("AggregationMap", AggregationMap.class, classMap);
		final List<ReferenceMap> referenceMaps = this.readObjects("ReferenceMap", ReferenceMap.class, classMap);
		final List<LookupMap> lookupMaps = this.readObjects("LookupMap", LookupMap.class, classMap);
		classMap.getColumnMaps().addAll(columnMaps);
		classMap.getAggregationMaps().addAll(aggregationMaps);
		classMap.getReferenceMaps().addAll(referenceMaps);
		classMap.getLookupMaps().addAll(lookupMaps);

		final List<NameValuePair> classMapKeys = this.mapper.flattenKeys(classMap);
		for (final AggregationMap aggregationMap : aggregationMaps) {
			final List<NameValuePair> aggregationMapKeys = mapper.flattenReferenceKeys(ComponentMap.class,	aggregationMap);
			aggregationMapKeys.addAll(classMapKeys);
			final List<ComponentMap> componentMaps = this.readObjects("ComponentMap", ComponentMap.class,aggregationMapKeys);
			if (!componentMaps.isEmpty() && (componentMaps.size() == 1)) {
				aggregationMap.setComponentMap(componentMaps.get(0));
			}
		}
		for (final ReferenceMap referenceMap : referenceMaps) {
			final List<NameValuePair> referenceMapKeys = mapper.flattenReferenceKeys(ReferenceKey.class,referenceMap);
			referenceMapKeys.addAll(classMapKeys);
			final List<ReferenceKey> referenceKeys = this.readObjects("ReferenceKey", ReferenceKey.class,referenceMapKeys);
			referenceMap.getReferenceKeys().addAll(referenceKeys);
		}
		for (final LookupMap lookupMap : lookupMaps) {
			final List<NameValuePair> referenceMapKeys = mapper.flattenReferenceKeys(LookupKey.class, lookupMap);
			referenceMapKeys.addAll(classMapKeys);
			final List<LookupKey> referenceKeys = this.readObjects("LookupKey", LookupKey.class, referenceMapKeys);
			lookupMap.getReferenceKeys().addAll(referenceKeys);
		}
	}

	@Override
	public Object lookupObject(String sheetName, List<NameValuePair> lookupKeys) {
		// TODO Auto-generated method stub
		return null;
	}
	


}
