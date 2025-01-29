package fw2.model2.xlsx.io;


import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.poi.ss.usermodel.PrintSetup;
import org.apache.poi.ss.usermodel.Sheet;

import fw2.model2.Aggregation;
import fw2.model2.ClassOrm;
import fw2.model2.Component;
import fw2.model2.Lookup;
import fw2.model2.Primitive;
import fw2.model2.Reference;
import fw2.orm.xlsx.io.NameValuePair;
import fw2.orm.xlsx.io.SheetLayout;
import fw2.orm.xlsx.io.SheetWriter;
import fw2.orm.xlsx.io.WorkbookLayout;
import fw2.orm.xlsx.io.WorkbookWriter;
import fw2.orm.xlsx.io.mapper.Mapper;

public class ClassOrmXlsxWriter extends WorkbookWriter<ClassOrm> {
	private class ClassMapSheetWriter extends SheetWriter {
		public ClassMapSheetWriter(final Sheet sheet, final Mapper mapper) {
			super(sheet, mapper);
			ClassOrmXlsxWriter.this.layout.addSheetLayout(this.layout); // Hack
			// for
			// style
		}

		public ClassMapSheetWriter(final Sheet sheet, final SheetLayout layout, final Mapper mapper) {
			super(sheet, layout, mapper);
		}
	}

	public ClassOrmXlsxWriter(final String fileName, final Mapper mapper) {
		super(fileName, mapper);
	}

	public ClassOrmXlsxWriter(final String fileName, final WorkbookLayout layout, final Mapper mapper) {
		super(fileName, layout, mapper);
	}

	public ClassMapSheetWriter createSheetWriter(final String sheetName) {
		Sheet sheet;
		sheet = this.workBook.createSheet(sheetName);
		final PrintSetup printSetup = sheet.getPrintSetup();
		printSetup.setLandscape(true);
		sheet.setFitToPage(true);
		sheet.setHorizontallyCenter(true);
		final ClassMapSheetWriter sheetWriter = new ClassMapSheetWriter(sheet, mapper);
		
		return sheetWriter;
	}

	@Override
	public void writeObject(final ClassOrm object) throws Exception {
		final ArrayList<ClassOrm> arrayList = new ArrayList<ClassOrm>();
		arrayList.add(object);
		this.writeObjects(arrayList);
	}

	@Override
	public void writeObjects(final List<ClassOrm> classOrms) throws Exception {
		try {
			final ClassMapSheetWriter classMapSheetWriter = this.createSheetWriter("ClassOrm");
			final ClassMapSheetWriter columnMapSheetWriter = this.createSheetWriter("Column");
			final ClassMapSheetWriter aggregationSheetWriter = this.createSheetWriter("Aggregation");
			final ClassMapSheetWriter componentMapSheetWriter = this.createSheetWriter("Component");
			final ClassMapSheetWriter referenceSheetWriter = this.createSheetWriter("Reference");
			final ClassMapSheetWriter referenceKeySheetWriter = this.createSheetWriter("ReferenceKey");
			final ClassMapSheetWriter lookupSheetWriter = this.createSheetWriter("Lookup");
			final ClassMapSheetWriter lookupKeySheetWriter = this.createSheetWriter("LookupKey");
			for (final ClassOrm map : classOrms) {
				classMapSheetWriter.writeRow(this.flatten(map));
				columnMapSheetWriter.writeRows(this.flattenPrimitives(map));
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

	protected List<NameValuePair> flatten(final ClassOrm classORM) throws Exception {
		return mapper.flatten((Object) classORM);
	}

	protected List<List<NameValuePair>> flattenAggregationMaps(final ClassOrm classORM) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final List<Aggregation> aggregations = classORM.getAggregations();
		for (final Aggregation aggregation : aggregations) {
			final List<NameValuePair> flattened = mapper.flatten(aggregation, classORM);
			result.add(flattened);
		}
		return result;
	}

	protected List<List<NameValuePair>> flattenComponentsMaps(final ClassOrm classORM) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final List<Aggregation> aggregations = classORM.getAggregations();
		for (final Aggregation aggregation : aggregations) {
			
			final List<NameValuePair> flattenedClassMapKeys = this.mapper.flattenKeys(aggregation, classORM);

			final Component component = aggregation.getComponent();
			final List<Primitive> maps = component.getPrimitives();
			for (final Primitive column : maps) {
				final List<NameValuePair> flattenedAggregation = new ArrayList<NameValuePair>();
				flattenedAggregation.addAll(flattenedClassMapKeys);
				final List<NameValuePair> flattenedColumnMap = mapper.flatten(column, component);
				flattenedAggregation.addAll(flattenedColumnMap);
				result.add(flattenedAggregation);
			}
		}
		return result;
	}

	protected List<List<NameValuePair>> flattenLookupKeys(final ClassOrm classORM) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		// Collection<Lookup> lookups = classORM.getLookups();
		// List<NameValuePair> classMapKeys = flattenKeys(classORM);
		// for (Reference lookupMap : lookups) {
		// Collection<Key> referenceKeys = lookupMap.getReferenceKeys();
		// for (Key key : referenceKeys) {
		// List<NameValuePair> flattened = new ArrayList<NameValuePair>();
		// flattened.addAll(classMapKeys);
		// flattened.addAll(this.flatten(key, lookupMap));
		// result.add(flattened);
		// }
		// }
		return result;
	}

	protected List<List<NameValuePair>> flattenLookupMaps(final ClassOrm classORM) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final Collection<Lookup> lookups = classORM.getLookups();
		for (final Lookup lookup : lookups) {
			final List<NameValuePair> flattened = mapper.flatten(lookup, classORM);
			result.add(flattened);
		}
		return result;
	}

	protected List<List<NameValuePair>> flattenPrimitives(final ClassOrm classORM) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final List<Primitive> columns = classORM.getPrimitives();
		for (final Primitive column : columns) {
			final List<NameValuePair> flattened = mapper.flatten(column, classORM);
			result.add(flattened);
		}
		return result;
	}

	protected List<List<NameValuePair>> flattenReferenceKeys(final ClassOrm classORM) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		// Collection<Reference> references = classORM.getReferences();
		// List<NameValuePair> classMapKeys = flattenKeys(classORM);
		//
		// for (Reference reference : references) {
		// Collection<Key> referenceKeys = reference.getReferenceKeys();
		// for (Key key : referenceKeys) {
		// List<NameValuePair> flattened = new ArrayList<NameValuePair>();
		// flattened.addAll(classMapKeys);
		// flattened.addAll(this.flatten(key, reference));
		// result.add(flattened);
		// }
		// }
		return result;
	}

	protected List<List<NameValuePair>> flattenReferenceMaps(final ClassOrm classORM) throws Exception {
		final List<List<NameValuePair>> result = new ArrayList<List<NameValuePair>>();
		final Collection<Reference> references = classORM.getReferences();
		for (final Reference reference : references) {
			final List<NameValuePair> flattened = mapper.flatten(reference, classORM);
			result.add(flattened);
		}
		return result;
	}
}
