package fw2.model2.xlsx.io;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.poi.ss.usermodel.Sheet;

import fw2.model2.Aggregation;
import fw2.model2.ClassOrm;
import fw2.model2.Component;
import fw2.model2.DbColumn;
import fw2.model2.DbDataSet;
import fw2.model2.DbForeignKeyColumn;
import fw2.model2.DbJoinKey;
import fw2.model2.DbJoinParameter;
import fw2.model2.DbRelation;
import fw2.model2.DbTable;
import fw2.model2.DbView;
import fw2.model2.DbViewColumn;
import fw2.model2.DomainAttribute;
import fw2.model2.DomainClass;
import fw2.model2.Lookup;
import fw2.model2.Primitive;
import fw2.model2.Property;
import fw2.model2.Reference;
import fw2.model2.TypeExtension;
import fw2.orm.xlsx.io.Factory;
import fw2.orm.xlsx.io.InstanceFactory;
import fw2.orm.xlsx.io.NameValuePair;
import fw2.orm.xlsx.io.SheetReader;
import fw2.orm.xlsx.io.WorkbookReader;
import fw2.orm.xlsx.io.mapper.Mapper;

public class ClassOrmXlsxReader extends WorkbookReader<ClassOrm> {
	private class ClassMapSheetReader extends SheetReader {
		public ClassMapSheetReader(final Sheet sheet, final Mapper mapper) {
			super(sheet, mapper);
		}

		@Override
		public <T> InstanceFactory getInstanceFactory(final Class<T> type) {
			if (type == Component.class) {
				if (ClassOrmXlsxReader.this.componentFactory == null) {
					ClassOrmXlsxReader.this.componentFactory = new ComponentFactory(ClassOrmXlsxReader.this.mapper);
				}
				return ClassOrmXlsxReader.this.componentFactory;
			} else if (type == DbDataSet.class) {
				if (ClassOrmXlsxReader.this.dbDataSetFactory == null) {
					ClassOrmXlsxReader.this.dbDataSetFactory = new DbDataSetFactory(ClassOrmXlsxReader.this.mapper);
				}
				return ClassOrmXlsxReader.this.dbDataSetFactory;
			} else if (type == Primitive.class) {
				if (ClassOrmXlsxReader.this.primitiveFactory == null) {
					ClassOrmXlsxReader.this.primitiveFactory = new PrimitiveFactory(ClassOrmXlsxReader.this.mapper);
				}
				return ClassOrmXlsxReader.this.primitiveFactory;
			} else {
				if (ClassOrmXlsxReader.this.factory == null) {
					ClassOrmXlsxReader.this.factory = new Factory(ClassOrmXlsxReader.this.mapper);
				}
				return ClassOrmXlsxReader.this.factory;
			}
		}
	}

	ComponentFactory componentFactory;
	DbDataSetFactory dbDataSetFactory;
	Factory factory;
	PrimitiveFactory primitiveFactory;

	public ClassOrmXlsxReader(final InputStream workbookInputStream, final Mapper mapper) throws IOException {
		super(workbookInputStream, mapper);
	}

	public List<DomainClass> readDomainClassObjects() throws Exception {
		final List<DomainClass> domainClasses = this.readDomainClasses();
		for (final DomainClass definition : domainClasses) {
			final List<DomainAttribute> properties = this.readObjects("DomainAttribute", DomainAttribute.class,
					definition);
			definition.getAttributes().addAll(properties);
		}
		return domainClasses;
	}

	@Override
	public List<ClassOrm> readObjects() throws Exception {

		final List<DomainClass> domainClasses = this.readDomainClasses();

		mapper.addLookupObjects("DomainClass", domainClasses);

		for (final DomainClass definition : domainClasses) {
			final List<DomainAttribute> properties = this.readObjects("DomainAttribute", DomainAttribute.class,
					definition);
			definition.getAttributes().addAll(properties);
		}
		final List<TypeExtension> extendedTypes = this.readExtendedTypes();

		mapper.addLookupObjects("TypeExtension", extendedTypes);

		for (final TypeExtension type : extendedTypes) {
			final List<Property> values = this.readObjects("Property", Property.class, type);
			type.getProperties().addAll(values);
		}
		final List<DbDataSet> dataSets = this.readDataSets();
		
		final List<DbTable> dbTables = new ArrayList<DbTable>();
		
		final List<DbView> dbViews = new ArrayList<DbView>();
		
		for (DbDataSet set: dataSets){
			if (set instanceof DbTable){
				dbTables.add((DbTable) set);
			}
		}

		for (DbDataSet set: dataSets){
			if (set instanceof DbView){
				dbViews.add((DbView) set);
			}
		}
		
		mapper.addLookupObjects("DbDataSet", dataSets);
		
		mapper.addLookupObjects("DbView", dbViews);	
		
		mapper.addLookupObjects("DbTable", dbTables);

		final List<DbRelation> relations = this.readDBRelations();

		mapper.addLookupObjects("DbRelation", relations);
		

		for (final DbRelation relation : relations) {
			final List<DbForeignKeyColumn> foreignKeyColumns = this.readObjects("DbForeignKeyColumn",DbForeignKeyColumn.class, relation);
			relation.getForeignKeyColumns().addAll(foreignKeyColumns);
		}
		for (final DbDataSet dataSet : dataSets) {
			this.populateDBDataSet(dataSet);
		}
		final Sheet classMapSheet = this.workBook.getSheet("ClassOrm");
		final ClassMapSheetReader reader = new ClassMapSheetReader(classMapSheet, this.mapper);
		final List<ClassOrm> ormPackages = reader.readObjects(ClassOrm.class);
		mapper.addLookupObjects("ClassOrm", ormPackages);

		for (final ClassOrm pacakage : ormPackages) {
			this.populateClassMap(pacakage);
		}
		return ormPackages;
	}

	@Override
	public List<ClassOrm> readObjects(final List<NameValuePair> keys) throws Exception {
		final Sheet sheet = this.workBook.getSheet("ClassOrm");
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		final List<ClassOrm> ormPackages = reader.readObjects(ClassOrm.class, keys);
		for (final ClassOrm pacakage : ormPackages) {
			this.populateClassMap(pacakage);
		}
		return ormPackages;
	}

	public <T> List<T> readObjects(final String sheetName, final Class<T> type) throws Exception {
		final Sheet sheet = this.workBook.getSheet(sheetName);
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		return reader.readObjects(type);
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

	private void populateClassMap(final ClassOrm classORM) throws Exception {
		final List<Primitive> columns = this.readObjects("Primitive", Primitive.class, classORM);
		final List<Aggregation> aggregations = this.readObjects("Aggregation", Aggregation.class, classORM);
		final List<Reference> references = this.readObjects("Reference", Reference.class, classORM);
		final List<Lookup> lookups = this.readObjects("Lookup", Lookup.class, classORM);
		classORM.getPrimitives().addAll(columns);
		classORM.getAggregations().addAll(aggregations);
		classORM.getReferences().addAll(references);
		classORM.getLookups().addAll(lookups);
		final List<NameValuePair> classMapKeys = this.mapper.flattenKeys(classORM);
		for (final Aggregation aggregation : aggregations) {
			final List<NameValuePair> aggregationMapKeys = this.mapper.flattenReferenceKeys(Component.class,
					aggregation);
			aggregationMapKeys.addAll(classMapKeys);
			final List<Component> components = this.readObjects("Component", Component.class, aggregationMapKeys);
			if (!components.isEmpty() && (components.size() == 1)) {
				aggregation.setComponent(components.get(0));
			}
		}
	}

	private void populateDBDataSet(final DbDataSet dataSet) throws Exception {
		if (dataSet instanceof DbTable) {
			final DbTable table = ((DbTable) dataSet);
			final List<DbColumn> columns = this.readObjects("DbColumn", DbColumn.class, table);
			table.getColumns().addAll(columns);
		} else {
			final DbView view = ((DbView) dataSet);
			final List<DbViewColumn> viewColumns = this.readObjects("DbViewColumn", DbViewColumn.class, view);
			final List<DbJoinKey> joinKeys = this.readObjects("DbJoinKey", DbJoinKey.class, view);
			final List<DbJoinParameter> joinParameters = this.readObjects("DbJoinParameter", DbJoinParameter.class,	view);
			view.getViewColumns().addAll(viewColumns);
			view.getJoinKeys().addAll(joinKeys);
			view.getJoinParameters().addAll(joinParameters);
		}
	}

	private List<DbDataSet> readDataSets() throws Exception {
		final Sheet sheet = this.workBook.getSheet("DbDataSet");
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		final List<DbDataSet> dataSets = reader.readObjects(DbDataSet.class);
		return dataSets;
	}

	private List<DbRelation> readDBRelations() throws Exception {
		final Sheet sheet = this.workBook.getSheet("DbRelation");
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		final List<DbRelation> relations = reader.readObjects(DbRelation.class);
		return relations;
	}

	private List<DomainClass> readDomainClasses() throws Exception {
		final Sheet sheet = this.workBook.getSheet("DomainClass");
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		final List<DomainClass> DomainClasss = reader.readObjects(DomainClass.class);
		return DomainClasss;
	}

	private List<TypeExtension> readExtendedTypes() throws Exception {
		final Sheet sheet = this.workBook.getSheet("TypeExtension");
		final ClassMapSheetReader reader = new ClassMapSheetReader(sheet, this.mapper);
		final List<TypeExtension> extendedTypes = reader.readObjects(TypeExtension.class);
		return extendedTypes;
	}

	@Override
	public Object lookupObject(String sheetName, List<NameValuePair> lookupKeys) {
		// TODO Auto-generated method stub
		return null;
	}
}
