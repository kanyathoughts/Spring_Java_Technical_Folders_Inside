package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.ColumnMap;
import fw2.orm.xlsx.ComponentMap;
import fw2.orm.xlsx.io.mapper.Mapper;

public class ComponentMapFactory extends Factory {


	final Factory factory;

	public ComponentMapFactory(final Mapper mapper) {
		super(mapper);
		this.factory = new Factory(mapper);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> T createInstance(final Class<T> type, final List<NameValuePair> row) throws Exception {
		final ClassMap map = mapper.getClassMapFor(type.getSimpleName());

		final ColumnMap columnMap = factory.createInstance(ColumnMap.class, row);
		final ComponentMap componentMap = mapper.createInstance(ComponentMap.class);
		this.mapper.populate(componentMap, map, row);
		componentMap.getColumnMaps().add(columnMap);
		return (T) componentMap;
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> List<T> createInstances(final Class<T> type, final List<ArrayList<NameValuePair>> rows)
			throws Exception {
		final List<T> result = new ArrayList<T>();
		final ClassMap map = mapper.getClassMapFor(type.getSimpleName());

		ComponentMap componentMap = null;
		for (final ArrayList<NameValuePair> row : rows) {
			final ComponentMap obj = mapper.createInstance(ComponentMap.class);
			this.mapper.populate(obj, map, row);
			final ColumnMap columnMap = factory.createInstance(ColumnMap.class,row);
			if (!result.contains(obj)) {
				componentMap = obj; 
				// To make sure columnMaps are added to the right component map!!
				componentMap.getColumnMaps().add(columnMap);
				result.add((T) componentMap);
			} else {
				Objects.requireNonNull(componentMap).getColumnMaps().add(columnMap);
			}
		}
		return result;
	}
}