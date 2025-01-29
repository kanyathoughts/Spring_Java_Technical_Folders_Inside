package fw2.model2.xlsx.io;

import java.util.ArrayList;
import java.util.List;

import fw2.model2.Component;
import fw2.model2.Primitive;
import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.io.Factory;
import fw2.orm.xlsx.io.NameValuePair;
import fw2.orm.xlsx.io.mapper.Mapper;

public class ComponentFactory extends Factory {
	final PrimitiveFactory factory;

	public ComponentFactory(final Mapper mapper) {
		super(mapper);
		this.factory = new PrimitiveFactory(mapper);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> T createInstance(final Class<T> type, final List<NameValuePair> row) throws Exception {
		final ClassMap map = mapper.getClassMapFor(type.getSimpleName());

		final Primitive columnMap = factory.createInstance(Primitive.class, row);
		final Component componentMap = mapper.createInstance(Component.class);
		this.mapper.populate(componentMap, map, row);
		componentMap.getPrimitives().add(columnMap);
		return (T) componentMap;
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> List<T> createInstances(final Class<T> type, final List<ArrayList<NameValuePair>> rows)
			throws Exception {
		final List<T> result = new ArrayList<T>();
		final ClassMap map = mapper.getClassMapFor(type.getSimpleName());

	
		for (final ArrayList<NameValuePair> row : rows) {
			Component temp = mapper.createInstance(Component.class);
			this.mapper.populate(temp, map, row);
			
			Component existing = getComponent(result, temp.getName());
			
			if (existing==null){
				result.add((T) temp);
				existing=temp;
			}
			final Primitive columnMap = factory.createInstance(Primitive.class,row);
			existing.getPrimitives().add(columnMap);
		
	
		}
		return result;
	}

	private <T> Component getComponent(final List<T> result, String name) {

		Component obj =null;
		for (T c: result){
			if (((Component)c).getName().equals(name)){
				obj= (Component) c;
				break;
			}
		}
		return obj;
	}

}