package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.List;

import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.io.mapper.Mapper;


public class Factory implements InstanceFactory {
	protected Mapper mapper;
	


	public Factory( Mapper mapper) {
		super();
		this.mapper = mapper;
	}

	@Override
	public <T> T createInstance(final Class<T> type, final List<NameValuePair> row) throws Exception {
		final ClassMap map = this.mapper.getClassMapFor(type);
		final T result = createInstance(type, row, map);
		return result;
	}

	protected <T> T createInstance(final Class<T> type, final List<NameValuePair> row, final ClassMap map) {
		final T result = createInstance(type);
		this.mapper.populate(result, map, row);
		return result;
	}

	protected <T> T createInstance(final Class<T> type) {
		final T result = this.mapper.createInstance(type);
		return result;
	}

	@Override
	public <T> List<T> createInstances(final Class<T> type, final List<ArrayList<NameValuePair>> rows)
			throws Exception {
		final List<T> result = new ArrayList<T>();
		final ClassMap map = this.mapper.getClassMapFor(type);
		for (final List<NameValuePair> row : rows) {
			final T obj = createInstance(type, row, map);
			result.add(obj);
		}
		return result;
	}
}