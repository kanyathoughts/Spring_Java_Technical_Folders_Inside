package fw2.orm.xlsx.io.mapper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.beanutils.PropertyUtils;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;

import fw2.orm.xlsx.AggregationMap;
import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.ColumnMap;
import fw2.orm.xlsx.ComponentMap;
import fw2.orm.xlsx.LookupMap;
import fw2.orm.xlsx.MapElement;
import fw2.orm.xlsx.ReferenceKey;
import fw2.orm.xlsx.ReferenceMap;
import fw2.orm.xlsx.io.NameValuePair;
import fw2.orm.xlsx.io.Repository;
import fw2.orm.xlsx.io.WorkbookReader;

public class Mapper {

	private Repository repository;
	
	private LookupStrategy lookupStrategy;

	public Mapper(final Repository repository, LookupStrategy lookupStrategy) {
		super();
		this.repository = repository;
		this.lookupStrategy = lookupStrategy;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void addLookupObjects(final String name, final List objects) {

		this.repository.addLookupObjects(name, objects);
	}

	public <T> T createInstance(final Class<T> type) {

		return this.repository.createInstance(type);
	}

	public List<NameValuePair> getColumns(final ClassMap classMap) {
		List<NameValuePair> columns = new ArrayList<NameValuePair>();
		ClassMap parentClassMap = this.getParentClass(classMap);
		if (parentClassMap!=null){
			final List<NameValuePair> referencesKeys = this.getReferenceKeys( parentClassMap, classMap);
			columns.addAll(referencesKeys);
		}
		columns = this.collectColumns(classMap, columns);
		return columns;
	}
	
	public List<NameValuePair> getReferenceKeys( final ClassMap classMap, final ClassMap relatedClassMap) {
		final ReferenceMap referenceMap = this.getReferenceMapFor(classMap, relatedClassMap);
		final List<NameValuePair> references = this.getReferenceKeys(classMap, referenceMap);
		return references;
	}

	private List<NameValuePair> getReferenceKeys(final ClassMap classMap, final ReferenceMap referenceMap) {
		final List<ColumnMap> keys = this.getKeyColumnMaps(classMap);
		final List<NameValuePair> translatedKeys = new ArrayList<NameValuePair>();
		final List<ReferenceKey> refereneKeys = referenceMap.getReferenceKeys();
		for (final ReferenceKey referenceKey : refereneKeys) {
			for (final ColumnMap colMap : keys) {
				if (referenceKey.getName().equals(colMap.getColumnName())) {
					final NameValuePair reference = new NameValuePair(referenceKey.getKeyColumnName(), colMap.getName());
					translatedKeys.add(reference);
				}
			}
		}
		return translatedKeys;
	}
	
	public List<NameValuePair> collectColumns(ClassMap classMap, List<NameValuePair> columns) {
		columns = this.collectColumns((ComponentMap) classMap, columns);
		columns = this.collectAggregateColumns(classMap, columns);
		return columns;

	}

	private List<NameValuePair> collectAggregateColumns(ClassMap classMap, List<NameValuePair> columns) {
		final List<AggregationMap> aggregates = classMap.getAggregationMaps();
		for (final AggregationMap aggregationMap : aggregates) {
			columns = this.collectAggregateColumns( aggregationMap, columns);
		}
		return columns;
	}

	private List<NameValuePair> collectAggregateColumns(AggregationMap aggregationMap, List<NameValuePair> columns) {
		return this.collectColumns(aggregationMap.getComponentMap(), columns);
	
	}

	private List<NameValuePair> collectColumns(ComponentMap map, List<NameValuePair> columns) {
		columns = this.collectPrimitiveColumns(map, columns);
		columns = this.collectLookupColumns(map, columns);
		return columns;
	}

	private List<NameValuePair> collectLookupColumns(ComponentMap map, List<NameValuePair> columns) {

		for (final LookupMap lookupMap : map.getLookupMaps()) {
			columns = this.collectLookupColumns(lookupMap, columns);
		}
		return columns;
	}

	private List<NameValuePair> collectLookupColumns(LookupMap lookupMap, List<NameValuePair> columns) {

		final List<ReferenceKey> refereneKeys = lookupMap.getReferenceKeys();
		for (final ReferenceKey referenceKey : refereneKeys) {
			final NameValuePair lookup = new NameValuePair(referenceKey.getName(), lookupMap.getName());
			columns.add(lookup);
		}

		return columns;
	}

	private List<NameValuePair> collectPrimitiveColumns(ComponentMap map, List<NameValuePair> columns) {
		for (final ColumnMap columnMap : map.getColumnMaps()) {
			columns.add(new NameValuePair(columnMap.getColumnName(), map.getName()+"."+columnMap.getName()));
		}
		return columns;
	}

	public List<NameValuePair> flatten(final Object object) {

		List<NameValuePair> result = new ArrayList<NameValuePair>();
		final ClassMap classMap = this.getClassMapFor(object);
		result = this.flatten(object, classMap, result);
		return result;
	}

	public List<NameValuePair> flatten(final Object object, final Object parentObject) {

		final ClassMap classMap = this.getClassMapFor(object);
		return this.flatten(object, parentObject, classMap);
	}

	public List<NameValuePair> flattenKeys(final Object object) {

		final List<NameValuePair> result = new ArrayList<NameValuePair>();
		final ClassMap classMap = this.getClassMapFor(object);
		if (classMap == null) {
			return result;
		}
		return this.flattenKeys(object, classMap);
	}

	private List<NameValuePair> flattenKeys(final Object object, final ClassMap map) {

		List<NameValuePair> result = new ArrayList<NameValuePair>();
		final List<ColumnMap> keys = this.getKeyColumnMaps(map);
		for (final ColumnMap key : keys) {
			result = this.flatten(object, key, result);
		}
		return result;
	}

	public List<NameValuePair> flattenKeys(final Object obj, final Object parentObject) throws Exception {

		final ClassMap classMap = this.getClassMapFor(obj);
		return this.flattenKeys(obj, parentObject, classMap);
	}
	

	public List<NameValuePair> flattenReferenceKeys(final EClass type, final Object parentObject) {

		final ClassMap classMap = this.getClassMapFor(parentObject);
		return this.flattenReferenceKeys(type, parentObject, classMap);
	}
	

	public List<NameValuePair> flattenReferenceKeys(final Class<?> type, final Object parentObject) {

		final ClassMap classMap = this.getClassMapFor(parentObject);
		return this.flattenReferenceKeys(type, parentObject, classMap);
	}

	public ClassMap getClassMapFor(final Class<?> type) {

		return this.repository.getClassMapFor(type);
	}

	public ClassMap getClassMapFor(final Object object) {

		return this.repository.getClassMapFor(object);
	}
	
	public ClassMap getClassMapFor(final EClass eClass) {

		return this.repository.getClassMapFor(eClass.getName());
	}

	public ClassMap getClassMapFor(final String name) {

		return this.repository.getClassMapFor(name);
	}

	public Object getProperty(final Object dataObject, final String propertyName) {

		return this.repository.getProperty(dataObject, propertyName);
	}

	public Repository getRepository() {

		return this.repository;
	}

	public void populate(final Object object, final ClassMap map, final List<NameValuePair> row) {

		this.populate(object, (ComponentMap) map, row);
		for (final AggregationMap aggregationMap : map.getAggregationMaps()) {
			this.populate(object, aggregationMap, row);
		}
	}

	public void setProperty(final Object object, final String name, final Object value) {

		this.repository.setProperty(object, name, value);

	}

	private List<NameValuePair> flatten(final Object object, final AggregationMap map, List<NameValuePair> flattened) {

		final Object component = this.read(object, map);
		flattened = this.flatten(component, map.getComponentMap(), flattened);
		return flattened;
	}

	private List<NameValuePair> flatten(final Object object, final ClassMap map, List<NameValuePair> result) {

		result = this.flatten(object, (ComponentMap) map, result);
		result = this.flattenAggregates(object, map, result);
		return result;
	}

	@SuppressWarnings("rawtypes")
	private class TreeIterator extends EcoreUtil.ContentTreeIterator {
		private static final long serialVersionUID = -1843886744030026632L;

		protected TreeIterator(Object object, boolean isResolveProxies) {
			super(object, isResolveProxies);
		}

	}
	TreeIterator iterator = new TreeIterator(Collections.EMPTY_LIST, true);
	
	public ClassMap getParentClass(ClassMap c) {
		final List<ClassMap> allClasses = allClasses(c);
		for (final ClassMap classMap : allClasses) {
			for (final ReferenceMap reference : classMap.getReferenceMaps()) {
				if ((null != reference.getRelatedClassMap()) && reference.getRelatedClassMap().equals(c)) {
					return classMap;
				}
			}
		}

		return null;
	}
	
	private List<ClassMap> allClasses(ClassMap c) {
		final List<ClassMap> allClasses = new ArrayList<ClassMap>();
		Iterator<?> children = iterator.getChildren(c.eContainer());
		while (children.hasNext()) {
			Object child = children.next();
			if (child instanceof ClassMap) {
				allClasses.add((ClassMap) child);
			}
		}
		return allClasses;
	}
	
	private List<NameValuePair> flatten(final Object object, final ColumnMap map, final List<NameValuePair> result) {

		String strVal = "";
		try {
			final Object value = this.getProperty(object, map.getName());
			if (value == null) {
				strVal = "";
			} else {
				strVal = value.toString();
			}

			result.add(new NameValuePair(map.getColumnName(), strVal));
		} catch (final Exception ex) {
			ex.printStackTrace();
		}
		return result;
	}

	private List<NameValuePair> flatten(final Object object, final ComponentMap map, List<NameValuePair> result) {

		result = this.flattenPrimitives(object, map, result);
		result = this.flattenLookups(object, map, result);
		return result;
	}

	private List<NameValuePair> flatten(final Object object, final LookupMap map, final List<NameValuePair> result) {

		final EObject lookupValue = (EObject) this.read(object, map);
		if (lookupValue == null ){
			return result;
		}
		final List<NameValuePair> flattenedLookupKeys = this.flattenKeys(lookupValue);
		final List<ReferenceKey> refereneKeys = map.getReferenceKeys();
		for (final ReferenceKey referenceKey : refereneKeys) {
			for (final NameValuePair element : flattenedLookupKeys) {
				if (referenceKey.getKeyColumnName().equals(element.getName())) {
					final NameValuePair lookup = new NameValuePair(referenceKey.getName(), element.getValue());
					result.add(lookup);
				}
			}
		}
		return result;
	}

	private List<NameValuePair> flatten(final Object object, final Object parentObject, final ClassMap classMap) {

		final ClassMap parentObjectClassMap = this.getClassMapFor(parentObject);
		final List<NameValuePair> referencesKeys = this.getReferenceKeys(parentObject, parentObjectClassMap, classMap);
		final List<NameValuePair> result = this.flatten(object, classMap, referencesKeys);
		return result;
	}

	private List<NameValuePair> flattenAggregates(final Object object, final ClassMap map, List<NameValuePair> flattened) {

		final List<AggregationMap> aggregates = map.getAggregationMaps();
		for (final AggregationMap aggregationMap : aggregates) {
			flattened = this.flatten(object, aggregationMap, flattened);
		}
		return flattened;
	}

	private List<NameValuePair> flattenKeys(final Object obj, final Object parentObject, final ClassMap classMap) throws Exception {

		final ClassMap parentObjectClassMap = this.getClassMapFor(parentObject);
		final List<NameValuePair> referencesKeys = this.getReferenceKeys(parentObject, parentObjectClassMap, classMap);
		final List<NameValuePair> flattenKeys = this.flattenKeys(obj);
		referencesKeys.addAll(flattenKeys);
		return referencesKeys;
	}

	private List<NameValuePair> flattenLookups(final Object object, final ComponentMap map, List<NameValuePair> result) {

		for (final LookupMap lookupMap : map.getLookupMaps()) {
			result = this.flatten(object, lookupMap, result);
		}
		return result;
	}

	private List<NameValuePair> flattenPrimitives(final Object object, final ComponentMap map, List<NameValuePair> result) {

		for (final ColumnMap columnMap : map.getColumnMaps()) {
			result = this.flatten(object, columnMap, result);
		}
		return result;
	}
	
	private List<NameValuePair> flattenReferenceKeys(final EClass type, final Object parentObject, final ClassMap classMap) {

		final ClassMap relatedClassMap = this.getClassMapFor(type);
		final List<NameValuePair> referencesKeys = this.getReferenceKeys(parentObject, classMap, relatedClassMap);

		return referencesKeys;
	}

	private List<NameValuePair> flattenReferenceKeys(final Class<?> type, final Object parentObject, final ClassMap classMap) {

		final ClassMap relatedClassMap = this.getClassMapFor(type);
		final List<NameValuePair> referencesKeys = this.getReferenceKeys(parentObject, classMap, relatedClassMap);

		return referencesKeys;
	}

	private List<ColumnMap> getKeyColumnMaps(final ComponentMap map) {

		final ArrayList<ColumnMap> keyColumns = new ArrayList<ColumnMap>();
		for (final ColumnMap columnMap : map.getColumnMaps()) {
			if (columnMap.isPrimaryKey()) {
				keyColumns.add(columnMap);
			}
		}
		return keyColumns;
	}

	private List<NameValuePair> getReferenceKeys(final Object parentObject, final ClassMap classMap, final ClassMap relatedClassMap) {

		final ReferenceMap referenceMap = this.getReferenceMapFor(classMap, relatedClassMap);
		final List<NameValuePair> references = this.getReferenceKeys(parentObject, classMap, referenceMap);
		return references;
	}

	private List<NameValuePair> getReferenceKeys(final Object parentObject, final ClassMap classMap, final ReferenceMap referenceMap) {

		final List<NameValuePair> keys = this.flattenKeys(parentObject, classMap);
		return this.translateKeys(referenceMap, keys);
	}

	private ReferenceMap getReferenceMapFor(final ClassMap classMap, final ClassMap relatedClassMap) {

		final Collection<ReferenceMap> maps = classMap.getReferenceMaps();
		for (final ReferenceMap map : maps) {
			if (map.getRelatedClassMap().getName().equals(relatedClassMap.getName())) {
				return map;
			}
		}
		return null;
	}

	private void populate(final Object object, final AggregationMap map, final List<NameValuePair> row) {

		try {

			final Object component = PropertyUtils.getProperty(object, map.getName());
			this.populate(component, map.getComponentMap(), row);

		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	private void populate(final Object object, final ColumnMap map, final List<NameValuePair> row) {

		for (final NameValuePair nameValuePair : row) {
			if (map.getColumnName().equals(nameValuePair.getName())) {
				try {
					final String value = nameValuePair.getValue();
					if ((value != null) && !value.trim().isEmpty()) {
						this.setProperty(object, map.getName(), value);
						//break;//verify
					}
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
		}
	}

	private void populate(final Object object, final ComponentMap map, final List<NameValuePair> row) {

		for (final ColumnMap columnMap : map.getColumnMaps()) {
			this.populate(object, columnMap, row);
		}
		for (final LookupMap lookupMap : map.getLookupMaps()) {
			this.populate(object, lookupMap, row);
		}
	}

	private void populate(final Object object, final LookupMap map, final List<NameValuePair> row) {

		final List<NameValuePair> lookupKeys = new ArrayList<NameValuePair>();
		final List<ReferenceKey> refereneKeys = map.getReferenceKeys();
		for (final ReferenceKey referenceKey : refereneKeys) {
			for (final NameValuePair value : row) {
				if (referenceKey.getName().equals(value.getName())) {
					final NameValuePair lookupKey = new NameValuePair(referenceKey.getKeyColumnName(), value.getValue());
					lookupKeys.add(lookupKey);
				}
			}
		}
		final Object value = this.lookupStrategy.lookupObject(map.getRelatedClassMap().getName(), lookupKeys);
		try {
			this.setProperty(object, map.getName(), value);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	private Object read(final Object element, final MapElement map) {

		Object propertyValue = null;
		try {
			final String name = map.getName();
			propertyValue = this.getProperty(element, name);
		} catch (final Exception ex) {
			ex.printStackTrace();
		}
		return propertyValue;
	}

	private List<NameValuePair> translateKeys(final ReferenceMap map, final List<NameValuePair> keys) {

		final List<NameValuePair> translatedKeys = new ArrayList<NameValuePair>();
		final List<ReferenceKey> refereneKeys = map.getReferenceKeys();
		for (final ReferenceKey referenceKey : refereneKeys) {
			for (final NameValuePair element : keys) {
				if (referenceKey.getName().equals(element.getName())) {
					final NameValuePair reference = new NameValuePair(referenceKey.getKeyColumnName(), element.getValue());
					translatedKeys.add(reference);
				}
			}
		}
		return translatedKeys;
	}

	@SuppressWarnings("rawtypes")
	public void initLookupStrategy(WorkbookReader workbookReader) {
		lookupStrategy.init(repository, workbookReader);
		
	}

	public List<NameValuePair> getColumns(String name) {
		return this.getColumns(this.getClassMapFor(name));

	}
}
