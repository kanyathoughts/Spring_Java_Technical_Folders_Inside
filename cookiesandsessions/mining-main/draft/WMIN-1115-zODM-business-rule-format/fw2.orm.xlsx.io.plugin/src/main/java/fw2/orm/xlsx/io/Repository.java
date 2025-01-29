package fw2.orm.xlsx.io;

import java.util.List;
import java.util.Map;

import fw2.orm.xlsx.ClassMap;

public interface Repository {

	<T> T createInstance(final Class<T> type);

	ClassMap getClassMapFor(Class<?> type);

	ClassMap getClassMapFor(Object object);

	ClassMap getClassMapFor(String simpleName);

	Object lookupObject(String name, List<NameValuePair> lookupKeys);
	
	void addLookupObjects(String name, List<Object> objects);

	Object getProperty(Object dataObject, String propertyName);

	void setProperty(Object object, String name, Object value);

	Map<String, List<?>> getLookupMap();
		
	ClassMap getParentClass(ClassMap classMap);

}
