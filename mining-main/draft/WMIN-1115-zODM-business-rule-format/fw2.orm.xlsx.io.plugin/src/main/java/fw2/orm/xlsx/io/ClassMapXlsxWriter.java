package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.poi.ss.usermodel.PrintSetup;
import org.apache.poi.ss.usermodel.Sheet;

import fw2.orm.xlsx.AggregationMap;
import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.ColumnMap;
import fw2.orm.xlsx.ComponentMap;
import fw2.orm.xlsx.LookupMap;
import fw2.orm.xlsx.ReferenceKey;
import fw2.orm.xlsx.ReferenceMap;
import fw2.orm.xlsx.io.mapper.Mapper;

public class ClassMapXlsxWriter extends WorkbookWriter<ClassMap> {
	private class ClassMapSheetWriter extends SheetWriter {
		public ClassMapSheetWriter(final Sheet sheet, final Mapper mapper) {
			super(sheet, mapper);
			ClassMapXlsxWriter.this.layout.addSheetLayout(this.layout); // Hack
			// for
			// style
		}

		public ClassMapSheetWriter(final Sheet sheet, final SheetLayout layout, final Mapper mapper) {
			super(sheet, layout, mapper);
		}
	}

	public ClassMapXlsxWriter(final String fileName, final Mapper mapper) {
		super(fileName, mapper);
	}

	public ClassMapXlsxWriter(final String fileName, final WorkbookLayout layout, final Mapper mapper) {
		super(fileName, layout, mapper);
	}

	public ClassMapSheetWriter createSheetWriter(final String sheetName) {
		Sheet sheet;
		sheet = this.workBook.createSheet(sheetName);
		final PrintSetup printSetup = sheet.getPrintSetup();
		printSetup.setLandscape(true);
		sheet.setFitToPage(true);
		sheet.setHorizontallyCenter(true);
		final ClassMapSheetWriter sheetWriter = new ClassMapSheetWriter(sheet, this.mapper);
		sheetWriter.setHeader(mapper.getColumns(sheetName));
		return sheetWriter;
	}

	@Override
	public void writeObject(final ClassMap object) throws Exception {
		final ArrayList<ClassMap> arrayList = new ArrayList<ClassMap>();
		arrayList.add(object);
		this.writeObjects(arrayList);
	}

	@Override
	public void writeObjects(final List<ClassMap> classMaps) throws Exception {
		try {
			final ClassMapSheetWriter classMapSheetWriter = this.createSheetWriter("ClassMap");
			final ClassMapSheetWriter columnMapSheetWriter = this.createSheetWriter("ColumnMap");
			final ClassMapSheetWriter aggregationSheetWriter = this.createSheetWriter("AggregationMap");
			final ClassMapSheetWriter componentMapSheetWriter = this.createSheetWriter("ComponentMap");
			final ClassMapSheetWriter referenceSheetWriter = this.createSheetWriter("ReferenceMap");
			final ClassMapSheetWriter referenceKeySheetWriter = this.createSheetWriter("ReferenceKey");
			final ClassMapSheetWriter lookupSheetWriter = this.createSheetWriter("LookupMap");
			final ClassMapSheetWriter lookupKeySheetWriter = this.createSheetWriter("LookupKey");
			for (final ClassMap map : classMaps) {
				classMapSheetWriter.writeRow(this.flatten(map));
				columnMapSheetWriter.writeRows(this.flattenColumnMaps(map));
				aggregationSheetWriter.writeRows(this.flattenAggregationMaps(map));
				componentMapSheetWriter.writeRows(this.flattenComponentsMaps(map));
				referenceSheetWriter.writeRows(this.flattenReferenceMaps(map));
				referenceKeySheetWriter.writeRows(this.flattenReferenceKeys(map));
				lookupSheetWriter.writeRows(this.flattenLookupMaps(map));
				lookupKeySheetWriter.writeRows(this.flattenLookupKeys(map));
			}
		} finally {
			this.close();
		}
	}

	protected List<NameValuePair> flatten(final ClassMap classMap) throws Exception {
		return this.mapper.flatten(classMap);
	}

	protected List<List<NameValuePair>> flattenAggregationMaps(final ClassMap classMap) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final List<AggregationMap> aggregationMaps = classMap.getAggregationMaps();
		for (final AggregationMap aggregationMap : aggregationMaps) {
			final List<NameValuePair> flattened = this.mapper.flatten(aggregationMap, classMap);
			result.add(flattened);
		}
		return result;
	}

	protected List<List<NameValuePair>> flattenColumnMaps(final ClassMap classMap) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final List<ColumnMap> columnMaps = classMap.getColumnMaps();
		for (final ColumnMap columnMap : columnMaps) {
			final List<NameValuePair> flattened = this.mapper.flatten(columnMap, classMap);
			result.add(flattened);
		}
		return result;
	}

	protected List<List<NameValuePair>> flattenComponentsMaps(final ClassMap classMap) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final List<AggregationMap> aggregationMaps = classMap.getAggregationMaps();
		for (final AggregationMap aggregationMap : aggregationMaps) {
			final List<NameValuePair> flattenedClassMapKeys = this.mapper.flattenKeys(aggregationMap, classMap);
			final ComponentMap componentMap = aggregationMap.getComponentMap();
			final List<ColumnMap> maps = componentMap.getColumnMaps();
			for (final ColumnMap columnMap : maps) {
				final List<NameValuePair> flattenedAggregation = new ArrayList<NameValuePair>();
				flattenedAggregation.addAll(flattenedClassMapKeys);
				final List<NameValuePair> flattenedColumnMap = this.mapper.flatten(columnMap, componentMap);
				flattenedAggregation.addAll(flattenedColumnMap);
				result.add(flattenedAggregation);
			}
		}
		return result;
	}

	protected List<List<NameValuePair>> flattenLookupKeys(final ClassMap classMap) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final Collection<LookupMap> lookupMaps = classMap.getLookupMaps();
		final List<NameValuePair> classMapKeys = this.mapper.flattenKeys(classMap);
		for (final ReferenceMap lookupMap : lookupMaps) {
			final Collection<ReferenceKey> referenceKeys = lookupMap.getReferenceKeys();
			for (final ReferenceKey referenceKey : referenceKeys) {
				final List<NameValuePair> flattened = new ArrayList<NameValuePair>();
				flattened.addAll(classMapKeys);
				flattened.addAll(this.mapper.flatten(referenceKey, lookupMap));
				result.add(flattened);
			}
		}
		return result;
	}

	protected List<List<NameValuePair>> flattenLookupMaps(final ClassMap classMap) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final Collection<LookupMap> lookupMaps = classMap.getLookupMaps();
		for (final LookupMap lookupMap : lookupMaps) {
			final List<NameValuePair> flattened = this.mapper.flatten(lookupMap, classMap);
			result.add(flattened);
		}
		return result;
	}

	protected List<List<NameValuePair>> flattenReferenceKeys(final ClassMap classMap) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final Collection<ReferenceMap> referenceMaps = classMap.getReferenceMaps();
		final List<NameValuePair> classMapKeys = this.mapper.flattenKeys(classMap);
		for (final ReferenceMap referenceMap : referenceMaps) {
			final Collection<ReferenceKey> referenceKeys = referenceMap.getReferenceKeys();
			for (final ReferenceKey referenceKey : referenceKeys) {
				final List<NameValuePair> flattened = new ArrayList<NameValuePair>();
				flattened.addAll(classMapKeys);
				flattened.addAll(this.mapper.flatten(referenceKey, referenceMap));
				result.add(flattened);
			}
		}
		return result;
	}

	protected List<List<NameValuePair>> flattenReferenceMaps(final ClassMap classMap) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final Collection<ReferenceMap> referenceMaps = classMap.getReferenceMaps();
		for (final ReferenceMap referenceMap : referenceMaps) {
			final List<NameValuePair> flattened = this.mapper.flatten(referenceMap, classMap);
			result.add(flattened);
		}
		return result;
	}
}
