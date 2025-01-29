package fw2.orm.xlsx.io;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class CollectionUtil {

	public static List<NameValuePair> add(final List<NameValuePair> list, final NameValuePair object) {
		final List<NameValuePair> result = new ArrayList<NameValuePair>();
		for (final NameValuePair entry : list) {
			if (!entry.getName().equals(object.getName())) {
				result.add(entry);
			}
		}
		result.add(object);
		return result;
	}

	public static <T> T car(final List<T> collection) {
		final Iterator<T> iter = collection.iterator();
		if (iter.hasNext()) {
			return iter.next();
		}
		return null;
	}

	public static <T> List<T> cdr(final List<T> collection) {
		final List<T> result = new ArrayList<T>();
		final Iterator<T> iter = collection.iterator();
		if (iter.hasNext()) {
			iter.next();
		}
		while (iter.hasNext()) {
			result.add(iter.next());
		}
		return result;
	}

	public static NameValuePair find(final List<NameValuePair> row, final String key) {
		for (final NameValuePair entry : row) {
			if (entry.getName().equals(key)) {
				return entry;
			}
		}
		return null;
	}

	public static List<NameValuePair> merge(final List<NameValuePair> list1, final List<NameValuePair> list2) {
		List<NameValuePair> result = new ArrayList<NameValuePair>();
		result.addAll(list2);
		for (final NameValuePair entry : list1) {
			result = CollectionUtil.add(result, entry);
		}
		return result;
	}
	
	public static String print(final List<NameValuePair> maps) {
		final StringBuffer result = new StringBuffer();
		for (final NameValuePair map : maps) {
			result.append("(");
			result.append(map.getName());
			result.append(":");
			result.append(map.getValue());
			result.append(")");
		}
		return result.toString();
	}

	public static boolean isValid(List<NameValuePair> lookupKeys) {
		return !(lookupKeys.isEmpty() || containsNulls(lookupKeys));

	}

	public static boolean containsNulls(List<NameValuePair> lookupKeys) {
		for (final NameValuePair map : lookupKeys) {
			if (map.getValue()==null) return true;
		}
		return false;
	}
}
