package fw2.model2.xlsx.io;

import java.util.ArrayList;
import java.util.List;

import fw2.model2.Constant;
import fw2.model2.DbColumnMap;
import fw2.model2.Primitive;
import fw2.model2.PrimitiveExtension;
import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.io.CollectionUtil;
import fw2.orm.xlsx.io.Factory;
import fw2.orm.xlsx.io.NameValuePair;
import fw2.orm.xlsx.io.mapper.Mapper;

public class PrimitiveFactory extends Factory {
	public PrimitiveFactory(final Mapper mapper) {
		super(mapper);
	}

	@Override
	public <T> T createInstance(final Class<T> type, final List<NameValuePair> row) throws Exception {
		final T obj = this.createDiscriminatedInstance(row);
		return obj;
	}

	@Override
	public <T> List<T> createInstances(final Class<T> type, final List<ArrayList<NameValuePair>> rows) throws Exception {
		final List<T> result = new ArrayList<T>();
		for (final List<NameValuePair> row : rows) {
			final T obj = this.createDiscriminatedInstance(row);
			result.add(obj);
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	private <T> T createDiscriminatedInstance(final List<NameValuePair> row) {
		final NameValuePair nameValuePair = CollectionUtil.find(row, "TYPE");
		final String type = nameValuePair.getValue();
		final ClassMap map = mapper.getClassMapFor(type);
		T obj = null;
		if ("DbColumnMap".equals(type)) {
			obj =(T) mapper.createInstance(DbColumnMap.class);
	
		} else if ("Constant".equals(type)) {
			obj =(T) mapper.createInstance(Constant.class);

		} else if ("Primitive".equals(type)) {
			obj =(T) mapper.createInstance(Primitive.class);
		
		} else {
			obj =(T) mapper.createInstance(PrimitiveExtension.class);
	
		}
		mapper.populate(obj, map,row);
		return obj;
	}
}